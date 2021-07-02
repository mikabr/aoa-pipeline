# library(broom)
# library(jglmm)

options(JULIA_HOME = "/Applications/Julia-1.5.app/Contents/Resources/julia/bin")
#jglmm_setup()

fit_bglm <- function(data, max_steps = 200) {
  model <- arm::bayesglm(cbind(num_true, num_false) ~ age,
                         family = "binomial",
                         prior.mean = .3,
                         prior.scale = c(.01),
                         prior.mean.for.intercept = 0,
                         prior.scale.for.intercept = 2.5,
                         prior.df = 1,
                         data = data,
                         maxit = max_steps)
  aoa <- -model$coefficients[["(Intercept)"]] / model$coefficients[["age"]]

  tibble(uni_lemma = data$uni_lemma[1],
         slope = model$coefficients[["age"]],
         aoa = aoa)
}

get_aoas <- function(data, max_steps = 200) {
  data |>
    split(~ uni_lemma) |>
    map(fit_bglm, max_steps) |>
    bind_rows()
}

# `df` must have the following columns:
#   language, measure,
#   lexical_category, uni_lemma,
#   prop, total, age
#   !!predictors
nest_data <- function(df, predictors, full = FALSE) {
  keep_data <- df |>
    mutate(group = paste(language, measure),
           lexical_category = lexical_category |> fct_relevel("other")) |>
    select(language, measure, group, lexical_category, item = uni_lemma, !!predictors)
  if (!full) { keep_data <- keep_data |> cbind(aoa = df$aoa) }
  keep_data |>
    group_by(language, measure) |>
    nest()
}

# when `full` is FALSE, `lex_effects` is ignored (default TRUE)
make_effs_formula <- function(predictors, full = FALSE, lex_effects = TRUE) {
  effs_age <- paste("age", predictors, sep = " * ")
  effs_lex <- paste("lexical_category", predictors, sep = " * ")
  ifelse(
    full,
    ifelse(
      lex_effects,
      glue("prop ~ (age | item) + {paste(c(effs_age, effs_lex), collapse = ' + ')}"),
      glue("prop ~ (age | item) + {paste(effs_age, collapse = ' + ')}")
    ),
    glue("aoa ~ {paste(c(effs_lex), collapse = ' + ')}")
  ) |> as.formula()
}

# `group_df` is the data output of `nest_data` for one group
fit_group_model <- function(group_df, predictors, formula,
                                 full = FALSE, contrasts = NULL) {
  group <- unique(group_df$group)
  message(glue("Fitting model for {group}..."))
  ifelse(
    full,
    return(jglmm(formula = formula, data = group_df, family = "binomial",
                 weights = group_df$total, contrasts = contrasts)),
    return(lm(formula = formula, data = group_df)) # not sure how to pass `contrasts` into `lm`
  )
}

# if `formula` is not specified, constructs one from `predictors` and `lex_effects`
fit_models <- function(predictors,df,  full = FALSE, lex_effects = TRUE, formula = NULL,
                       contrasts = list(lexical_category = "effects")) {
#  if (full & !lex_effects) { contrasts$lexical_category <- NULL }
#  if (length(contrasts) == 0) { contrasts <- NULL }
#  if (is.null(formula)) { formula <- make_effs_formula(predictors, full, lex_effects) }
  formula <- make_effs_formula(predictors, full, lex_effects)
  df <- df %>%
  select(language, measure, uni_lemma, aoa, items, final_frequency, frequency, solo_frequency, first_frequency, valence, concreteness, babiness, mlu, length_char, n_tokens, lexical_category) %>%
  unique()
  nest_data(df, predictors, full) |>
  mutate(model = map(data, ~ fit_group_model(.x, predictors, formula, full, contrasts)),
           results = map(model, tidy),
           rsquared = map(model, glance),
           preds = list(predictors))
}

####### TEST CASE
# load("data/temp_saved_data/uni_model_data.RData")
# uni_model_data <- uni_model_data |> ungroup()
#
# eng_model_data <- uni_model_data |>
#   filter(language == "English (American)") |>
#   mutate(total = num_true + num_false)
#
# aoas <- get_aoas(eng_model_data)
#
# eng_model_data <- eng_model_data |>
#   left_join(aoas, by = "uni_lemma")
#
# predictors <- list(
#   c("frequency", "MLU", "final_frequency", "solo_frequency"),
#   c("valence", "arousal"),
#   "concreteness", "babiness", "num_phons"
# ) |> unlist()
#
# eng_models <- fit_models(eng_model_data, predictors)
