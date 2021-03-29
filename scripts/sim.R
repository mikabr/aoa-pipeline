library(here)
library(jglmm)
library(ggplot2)
source(here("scripts", "prep_data.R"))

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
eng_wdbk <- readRDS(here("data/wordbank/english_(american).rds")) %>% as_tibble %>%
  merge(., eng_word %>% select(uni_lemma, prop))
eng_prop <- eng_word %>% mutate(total = num_true + num_false) %>% select(uni_lemma, prop, age, total) 
sim_results <- tibble()

# one simulation (i.e. one imputation)
sim_one <- function() {
  eng_impt <- eng_word %>% do_full_imputation(pred_sources, 20) %>% `[[`(3) %>% `[[`(1)
  eng_data <- merge(eng_prop, eng_impt, by = c("uni_lemma")) #%>% #, all.x = TRUE) %>%
    #mutate(prop = num_true / total)
  
  glm(effects_formula, data = eng_data, family = "binomial", weights = total) %>% .$coefficients
}

# 100 simulations
test <- replicate(100, sim_one()) %>% t() %>% as_tibble()
std_devs <- sapply(test, sd)

# data and graphical output
write_csv(test, "data/sim_data.csv")

test_long <- test %>% mutate(sim = row_number()) %>%
  pivot_longer(!sim, names_to = "coefficient")

dens_plot <- ggplot(data = test_long %>% filter(coefficient != "(Intercept)", !grepl(":", coefficient)), 
                    aes(x = coefficient, y = value)) + 
  geom_violin() +
  guides(x = guide_axis(angle = 45)) #+
  #stat_summary(fun.data = mean_cl_boot, geom = "pointrange", color = "red")

               