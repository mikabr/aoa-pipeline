# library(broom)
# library(jglmm)

options(JULIA_HOME = "/Applications/Julia-1.5.app/Contents/Resources/julia/bin")
jglmm_setup()

fit_aoa <- function(data) {
  model <- arm::bayesglm(cbind(num_true, num_false) ~ unscaled_age,
                         family = "binomial",
                         prior.mean = .3,
                         prior.scale = c(.01),
                         prior.mean.for.intercept = 0,
                         prior.scale.for.intercept = 2.5,
                         prior.df = 1,
                         data = data,
                         maxit = 200)
  aoa <- -model$coefficients[["(Intercept)"]] / model$coefficients[["unscaled_age"]]

  tibble(uni_lemma = data$uni_lemma[1],
         slope = model$coefficients[["unscaled_age"]],
         aoa = aoa)
}

get_aoas <- function(data) {
  data |>
    split(~ uni_lemma) |>
    map(fit_aoa) |>
    bind_rows()
}

# `df` must have the following columns:
#   language, measure,
#   lexical_category, uni_lemma,
#   prop, total, age
#   !!predictors
nest_data <- function(df, predictors, full = FALSE) {
  df |> mutate(group = paste(language, measure),
               lexical_category = lexical_category |> fct_relevel("other")) |>
    select(language, measure, group, lexical_category, item = uni_lemma, prop,
           total, age, !!predictors, ifelse(full, NULL, "aoa")) |>
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
    return(lm(formula = formula, data = group_df,
              weights = total)) # not sure how to pass `contrsts` into `lm`
  )
}

# if `formula` is not specified, constructs one from `predictors` and `lex_effects`
fit_models <- function(df, predictors, full = FALSE, lex_effects = TRUE, formula = NULL,
                       contrasts = list(lexical_category = "effects")) {
  if (full & !lex_effects) { contrasts$lexical_category <- NULL }
  if (length(contrasts) == 0) { contrasts <- NULL }
  if (is.null(formula)) { formula <- make_effs_formula(predictors, full, lex_effects) }
  nest_data(df, predictors, full) |>
    mutate(model = map(data, ~ fit_group_model(.x, predictors, formula, full, contrasts)),
           results = map(model, tidy))
}

####### TEST CASE
load("data/temp_saved_data/uni_model_data.RData")
uni_model_data <- uni_model_data |> ungroup()

eng_model_data <- uni_model_data |>
  filter(language == "English (American)") |>
  mutate(total = num_true + num_false)

aoas <- get_aoas(eng_model_data)

eng_model_data <- eng_model_data |>
  left_join(aoas, by = "uni_lemma")

predictors <- list(
  c("frequency", "MLU", "final_frequency", "solo_frequency"),
  c("valence", "arousal"),
  "concreteness", "babiness", "num_phons"
) |> unlist()

eng_models <- fit_models(eng_model_data, predictors)
