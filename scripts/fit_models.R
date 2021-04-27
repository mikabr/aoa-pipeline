# library(broom)
# library(jglmm)

# options(JULIA_HOME = "/Applications/Julia-1.5.app/Contents/Resources/julia/bin")
# jglmm_setup()

# `df` must have the following columns:
#   language, measure,
#   lexical_category, uni_lemma,
#   prop, total, age
#   !!predictors
nest_data <- function(df, predictors) {
  df %>% mutate(group = paste(language, measure),
                lexical_category = lexical_category %>% fct_relevel("other")) %>%
    select(language, measure, group, lexical_category, item = uni_lemma, prop, 
           total, age, !!predictors) %>%
    group_by(language, measure) %>%
    nest()
}

make_effs_formula <- function(predictors, lex_effects = TRUE) {
  effs_age <- paste("age", predictors, sep = " * ")
  effs_lex <- paste("lexical_category", predictors, sep = " * ")
  ifelse(
    lex_effects, 
    glue("prop ~ (age | item) + {paste(c(effs_age, effs_lex), collapse = ' + ')}"),
    glue("prop ~ (age | item) + {paste(effs_age, collapse = ' + ')}")
  ) %>% as.formula()
}

# `group_data` is the data output of `nest_data` for one group
fit_group_model <- function(group_data, predictors, formula, contrasts = NULL) {
  group <- unique(group_data$group)
  message(glue("Fitting model for {group}..."))
  jglmm(formula = formula, data = group_data, family = "binomial",
        weights = group_data$total, contrasts = contrasts)
}

# if `formula` is not specified, constructs one from `predictors` and `lex_effects`
fit_models <- function(df, predictors, lex_effects = TRUE, formula = NULL, 
                       contrasts = list(lexical_category = "effects")) {
  if (!lex_effects) { contrasts$lexical_category <- NULL }
  if (length(contrasts) == 0) { contrasts <- NULL }
  if (is.null(formula)) { formula <- make_effs_formula(predictors, lex_effects) }
  nest_data(df, predictors) %>%
    mutate(model = data %>% map(~ fit_group_model(.x, predictors, formula, contrasts)),
           results = map(model, tidy))
}

####### TEST CASE
# load("data/temp_saved_data/uni_model_data.RData")
# uni_model_data <- uni_model_data %>% ungroup()
# 
# eng_model_data <- uni_model_data %>% 
#   filter(language == "English (American)") %>%
#   mutate(total = num_true + num_false)
# 
# predictors <- list(
#   c("frequency", "MLU", "final_frequency", "solo_frequency"),
#   c("valence", "arousal"), 
#   "concreteness", "babiness", "num_phons"
# ) %>% unlist()
# 
# eng_models <- fit_models(eng_model_data, predictors)
