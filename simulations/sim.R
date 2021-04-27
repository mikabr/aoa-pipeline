library(here)
library(stats)
library(ggplot2)
library(ggthemes)
library(magrittr)
source(here("scripts", "prep_data.R"))

theme_set(papaja::theme_apa(base_family = "Source Sans Pro"))
theme_update(strip.text.x = element_text(margin = margin(b = 8), face = "bold"),
             strip.text.y = element_text(margin = margin(l = 8), face = "bold"))

set.seed(1000)

# define effects formulas (note: only lex_effects_formula is used)
predictors <- unlist(pred_sources)

effects <- paste("age", predictors, sep = " * ")
effects_formula <- as.formula(
  glue("prop ~ {paste(effects, collapse = ' + ')}")
)
lex_effects <- paste("lexical_category", predictors, sep = " * ")
lex_effects_formula <- as.formula(
  glue("prop ~ {paste(c(effects, lex_effects), collapse = ' + ')}")
)

# get english data
new_test_data <- uni_joined %>%
  #select out just the by lexical item data
  distinct() %>%
  #pull out categories from classes
  mutate(lexical_category = if_else(str_detect(lexical_classes, ","), "other", lexical_classes),
         # collapse v, adj, adv into one category
         lexical_category = lexical_category %>% as_factor() %>% 
           fct_collapse("predicates" = c("verbs", "adjectives", "adverbs"))) %>%
  select(-lexical_classes)

eng_word <- new_test_data %>% filter(language == "English (American)") %>% as_tibble 
eng_wdbk <- readRDS(here("data/wordbank/english_(american).rds")) %>% as_tibble
eng_prop <- eng_word %>% mutate(total = num_true + num_false) %>% select(uni_lemma, measure, prop, age, total) 
eng_prop_und <- eng_prop %>% filter(measure == "understands")
eng_prop_pro <- eng_prop %>% filter(measure == "produces")

# one simulation (i.e. one imputation)
sim_one <- function(prop_df) {
  eng_impt <- eng_word %>% do_full_imputation(pred_sources, 20) %>% `[[`(3) %>% `[[`(1)
  eng_data <- merge(prop_df, eng_impt, by = c("uni_lemma"))
  
  glm(effects_formula, data = eng_data, family = "binomial", weights = total)# %>% .$coefficients
}

# 100 simulations
eng_sim_und <- replicate(100, sim_one(eng_prop_und), simplify = FALSE)
eng_sim_pro <- replicate(100, sim_one(eng_prop_pro), simplify = FALSE)

varnames <- c("(Intercept)", "age", predictors, paste("age:", predictors, sep = ""))
measurenames <- c("estimate", "std_error", "z_value", "p_value")

get_coefs_val <- function(sim_coefs) {
  coefs_data <- sim_coefs %>% as.data.frame() %>% rownames_to_column() %>%
    pivot_longer(!rowname, names_to = "measure")
  return(coefs_data$value)
}

get_coefs <- function(sim_df) {
  coefs <- lapply(sim_df, function(x) {summary(x) %>% .$coefficients})
  # a little bit hacky, but I couldn't figure out how to merge the lists properly otherwise
  sapply(coefs, get_coefs_val) %>% as.data.frame() %>%
    mutate(term = rep(varnames, each = 4),
           measure = rep(measurenames, times = 20)) %>%
    select(term, measure, everything()) %>%
    pivot_longer(-c(term, measure), names_to = "sim", names_prefix = "V") %>%
    pivot_wider(names_from = "measure", values_from = "value") %>%
    mutate(signif = (p_value < .05))
}

eng_sim_und_coefs <- get_coefs(eng_sim_und)
eng_sim_pro_coefs <- get_coefs(eng_sim_pro)

# data and graphical output
saveRDS(eng_sim_und, "data/eng_sim_und_data.rds")
saveRDS(eng_sim_pro, "data/eng_sim_pro_data.rds")

## coerce data into relevant format
bind_coefs <- function(und_coefs, pro_coefs) {
  rbind(und_coefs %>% mutate(measure = "Understands"),
        pro_coefs %>% mutate(measure = "Produces")) %>%
    mutate(effect = factor(ifelse(grepl(":", term), "Interaction with age", "Main"), 
                           levels = c("Main", "Interaction with age")),
           term = sub("age:", "", .$term)) %>% 
    filter(term != "(Intercept)", term != "age") %>%
    mutate(term = factor(term, levels = c("arousal", "valence", "final_frequency", "babiness", "MLU", 
                                          "solo_frequency", "concreteness", "num_phons", "frequency")),
           measure = factor(measure, levels = c("Understands", "Produces")))
}

eng_sim_coefs_full <- rbind(eng_sim_und_coefs %>% mutate(measure = "Understands"),
                            eng_sim_pro_coefs %>% mutate(measure = "Produces")) %>%
  mutate(effect = factor(ifelse(grepl(":", term), "Interaction with age", "Main"), 
                         levels = c("Main", "Interaction with age")),
         term = sub("age:", "", .$term)) %>% 
  filter(term != "(Intercept)", term != "age") %>%
  mutate(term = factor(term, levels = c("arousal", "valence", "final_frequency", "babiness", "MLU", 
                                   "solo_frequency", "concreteness", "num_phons", "frequency")),
         measure = factor(measure, levels = c("Understands", "Produces")))

term_labels = c("Arousal", "Valence", "Final frequency", "Babiness", "MLU-w", 
                "Solo frequency", "Concreteness", "Number of Phonemes", "Frequency")

coef_plot <- ggplot(eng_sim_coefs_full, aes(x = estimate, y = term)) +
  facet_grid(measure ~ effect, scales = "free",
             labeller = as_labeller(label_caps)) +
  geom_point(aes(colour = term), alpha = .2, size = .5, position = position_dodge2(width = .5)) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
  scale_colour_ptol(guide = FALSE) +
  scale_shape_manual(values = c(19, 21), guide = FALSE) +
  labs(y = "", x = "Coefficient estimate") + 
  theme(legend.position = "none") +
  scale_y_discrete(labels = term_labels) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, 
               geom = "errorbar", width = .75, aes(color = term))

ggsave(filename = "sim_new.png", width = 8, height = 5.6, device='png', dpi=300)

## add previously imputed data
load(here("data", "temp_saved_data", "uni_model_data.RData"))
uni_model_data %<>% ungroup()
eng_mod <- uni_model_data %>% filter(language == "English (American)") %>%
  mutate(total = num_true + num_false) # %>%
  # select(c(uni_lemma, measure, prop, age, total))
eng_mod_und <- eng_mod %>% filter(measure == "understands") %>% 
  glm(effects_formula, data = ., family = "binomial", weights = total)
eng_mod_pro <- eng_mod %>% filter(measure == "produces") %>% 
  glm(effects_formula, data = ., family = "binomial", weights = total)
eng_mod_und_coefs <- get_coefs(list(eng_mod_und))
eng_mod_pro_coefs <- get_coefs(list(eng_mod_pro))

eng_mod_coefs_full <- bind_coefs(eng_mod_und_coefs, eng_mod_pro_coefs)

coef_plot + geom_point(data = eng_mod_coefs_full, size = .7)
