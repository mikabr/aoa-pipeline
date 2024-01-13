# CDI -> AoA models

fit_bglm <- function(df, max_steps = 200) {
  model <- arm::bayesglm(cbind(num_true, num_false) ~ age,
                         family = "binomial",
                         prior.mean = .3,
                         prior.scale = c(.01),
                         prior.mean.for.intercept = 0,
                         prior.scale.for.intercept = 2.5,
                         prior.df = 1,
                         data = df,
                         maxit = max_steps)
  intercept <- model$coefficients[["(Intercept)"]]
  slope <- model$coefficients[["age"]]
  tibble(intercept = intercept, slope = slope, aoa = -intercept / slope)
}

fit_aoas <- function(wb_data, max_steps = 200, min_aoa = 0, max_aoa = 72) {
  aoas <- wb_data |>
    mutate(num_false = total - num_true) |>
    nest(data = -c(language, measure, uni_lemma)) |>
    mutate(aoas = map(data, fit_bglm)) |>
    dplyr::select(-data) |>
    unnest(aoas) |>
    filter(aoa >= min_aoa, aoa <= max_aoa)
}

# Word predictor models

make_predictor_formula <- function(predictors, lexcat_interactions = TRUE,
                                   morphcomp_interactions = FALSE,
                                   all_lang = FALSE) {
  predictors <- predictors |> as.character()
  if (lexcat_interactions) {
    predictors <- paste(predictors, "lexical_category", sep = " * ")
  }
  if (morphcomp_interactions) {
    predictors <- c(predictors,
                    paste(predictors, "morph_complexity", sep = " : "))
  }
  if (all_lang) {
    predictors <- c(predictors,
                    #paste("(", predictors, "|language)", sep = "")
                    "(1|language)"
                    )
  }
  glue("aoa ~ {paste(predictors, collapse = ' + ')}") |> as.formula()
}

fit_group_model <- function(predictors, group_data, lexcat_interactions = TRUE,
                            morphcomp_interactions = FALSE, all_lang = FALSE,
                            model_formula = NULL) {
  # discard predictors that data has no values for
  predictors <- drop_predictors(predictors, group_data)
  group_data <- group_data |>
    mutate(lexical_category = lexical_category |> fct_drop())
  if (is.null(model_formula)) {
    model_formula <- make_predictor_formula(predictors, lexcat_interactions,
                                            morphcomp_interactions, all_lang)
  }
  if (all_lang) {
    lmerTest::lmer(model_formula, group_data)
  } else {
    # lm(model_formula, group_data)
    # arm::bayesglm(model_formula,
    #               family = gaussian,
    #               data = group_data,
    #               prior.scale = 2,
    #               prior.df = 3,
    #               scaled = FALSE)
    brms::brm(model_formula,
              group_data,
              prior = brms::prior(student_t(3, 0, 2), class = "b"))
  }
}

get_vifs <- function(model) {
  if (class(model) == "brmsfit") {
    vif <- performance::check_collinearity(model)
  } else {
    vif <- car::vif(model)
  }
  as_tibble(vif) |> rownames_to_column("predictor") |> rename_with(tolower)
}

fit_models <- function(predictors, predictor_data, lexcat_interactions = TRUE,
                       model_formula = NULL) {
  sinotibetan_langs <- c("Mandarin (Beijing)", "Mandarin (Taiwanese)", "Cantonese")
  predictor_data |>
    nest(group_data = -c(language, measure)) |>
    mutate(predictors = ifelse(language %in% sinotibetan_langs,
                               list(setdiff(predictors, c("n_features", "form_entropy"))),
                               list(predictors)),
           model = map2(group_data, predictors,
                        \(gd, preds) fit_group_model(preds, gd, lexcat_interactions,
                                                     morphcomp_interactions = FALSE,
                                                     all_lang = FALSE, model_formula)),
           coefs = map(model, broom.mixed::tidy),
           stats = map(model, broom.mixed::glance),
           # alias = map(model, alias)# ,
           vifs = map(model, get_vifs)
    )
}

fit_all_lang_model <- function(predictors, predictor_data,
                               lexcat_interactions = TRUE,
                               morphcomp_interactions = FALSE,
                               model_formula = NULL) {
  predictor_data |>
    mutate(language = as.factor(language) |>
             `contrasts<-`(value = "contr.sum")) |>
    nest(group_data = -c(measure)) |>
    mutate(model = group_data |>
             map(\(gd) fit_group_model(predictors, gd, lexcat_interactions,
                                       morphcomp_interactions,
                                       all_lang = TRUE, model_formula)),
           coefs = map(model, broom::tidy),
           stats = map(model, broom::glance),
           # alias = map(model, alias)
           # vifs = map(model, get_vifs)
    )
}

