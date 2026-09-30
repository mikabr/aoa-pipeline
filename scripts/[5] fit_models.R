# CDI -> AoA models

aoa_se <- function(model) {
  coefs <- coef(model)
  a <- coefs[["(Intercept)"]]
  b <- coefs[["age"]]
  V <- vcov(model)[c("(Intercept)", "age"), c("(Intercept)", "age")]
  g <- c(-1 / b, a / b^2)
  se <- sqrt(as.numeric(t(g) %*% V %*% g))
  if (!is.finite(se) || se <= 0) NA_real_ else se
}

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
  tibble(intercept = intercept, slope = slope,
         aoa = -intercept / slope,
         aoa_se = aoa_se(model))
}

fit_aoas <- function(wb_data, max_steps = 200, min_aoa = 0, max_aoa = 72) {
  aoas <- wb_data |>
    mutate(num_false = total - num_true) |>
    nest(data = -c(language, measure, uni_lemma)) |>
    mutate(aoas = map(data, fit_bglm, .progress = "Fitting AoAs")) |>
    dplyr::select(-data) |>
    unnest(aoas) |>
    filter(aoa >= min_aoa, aoa <= max_aoa)
}

# Word predictor models

make_predictor_formula <- function(predictors, lexcat_interactions = TRUE,
                                   morphcomp_interactions = FALSE,
                                   all_lang = FALSE,
                                   measurement_error = FALSE) {
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
                    glue("({paste(predictors, collapse = ' + ')}|language)")
                    # "(1|language)"
                    )
  }
  lhs <- if (measurement_error) "aoa | se(aoa_se, sigma = TRUE)" else "aoa"
  glue("{lhs} ~ {paste(predictors, collapse = ' + ')}") |> as.formula()
}

fit_group_model <- function(predictors, group_data, lexcat_interactions = TRUE,
                            morphcomp_interactions = FALSE, all_lang = FALSE,
                            model_formula = NULL) {
  # discard predictors that data has no values for
  predictors <- drop_predictors(predictors, group_data)
  group_data <- group_data |>
    mutate(lexical_category = lexical_category |> fct_drop())
  if (is.null(model_formula)) {
    model_formula <- make_predictor_formula(
      predictors, lexcat_interactions, morphcomp_interactions, all_lang,
      measurement_error = "aoa_se" %in% names(group_data)
    )
  }
  if (all_lang) {
    # lmerTest::lmer(model_formula, group_data)
    brms::brm(model_formula,
              group_data,
              # prior = brms::prior(horseshoe(1), class = "b"),
              prior = brms::prior(student_t(3, 0, 2), class = "b"),
              control = list(adapt_delta = 0.95, max_treedepth = 12),
              init_r = 0.1,
              cores = if (parallel::detectCores() > 4) 4 else 1,
              iter = 4000)
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
              # prior = brms::prior(horseshoe(1), class = "b"),
              prior = brms::prior(student_t(3, 0, 2), class = "b"),
              cores = if (parallel::detectCores() > 4) 4 else 1,
              iter = 4000,
              refresh = 0)
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
                                                     all_lang = FALSE, model_formula),
                        .progress = TRUE),
           # coefs = map(model, broom.mixed::tidy),
           coefs = map(model, bayestestR::describe_posterior,
                       centrality = "MAP", ci_method = "HDI"),
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
           coefs = map(model, bayestestR::describe_posterior,
                       centrality = "MAP", ci_method = "HDI"),
           stats = map(model, broom.mixed::glance),
           # alias = map(model, alias)
           # vifs = map(model, get_vifs)
    )
}

coef_se_from_hdi <- function(conf.low, conf.high) {
  (conf.high - conf.low) / (2 * qnorm(0.975))
}

mc_term <- function(x) {
  x |> str_remove("^b_") |> str_replace("^Intercept$", "(Intercept)")
}

summarise_mc_model <- function(model) {
  if (inherits(model, "brmsfit")) {
    est <- broom.mixed::tidy(model, effects = "fixed") |>
      mutate(term = mc_term(term))
    hdi <- bayestestR::describe_posterior(
      model, centrality = "MAP", ci_method = "HDI"
    ) |>
      as_tibble() |>
      mutate(term = mc_term(Parameter)) |>
      select(term, ci.low = CI_low, ci.high = CI_high)
    est |>
      select(term, estimate, std.error) |>
      left_join(hdi, by = "term")
  } else {
    broom.mixed::tidy(model) |>
      mutate(ci.low = estimate - qnorm(0.975) * std.error,
             ci.high = estimate + qnorm(0.975) * std.error)
  }
}

mc_fit_lines <- function(mc_vals) {
  slopes <- mc_vals |>
    filter(term == "morph_complexity") |>
    mutate(reliability = ifelse(signif, "Reliable", "Not reliable")) |>
    select(measure, ms_term, slope = estimate, reliability)
  intercepts <- mc_vals |>
    filter(term == "(Intercept)") |>
    select(measure, ms_term, intercept = estimate)
  slopes |>
    left_join(intercepts, by = c("measure", "ms_term")) |>
    rename(term = ms_term)
}

mc_fit_bands <- function(mc_mods, length_out = 50) {
  mc_mods |>
    mutate(band = map(model, \(mod) {
      x_obs <- mod$data$morph_complexity
      x <- seq(min(x_obs), max(x_obs), length.out = length_out)
      draws <- brms::posterior_epred(
        mod, newdata = tibble(morph_complexity = x, coef_se = 0)
      )
      tibble(
        morph_complexity = x,
        ymin = apply(draws, 2, quantile, probs = 0.025),
        ymax = apply(draws, 2, quantile, probs = 0.975)
      )
    })) |>
    select(measure, term = ms_term, band) |>
    unnest(band)
}

fit_mc_models <- function(main_coefs, morph_complexity, terms,
                          measures = c("produces", "understands")) {
  if ("lexical_category" %in% names(main_coefs)) {
    main_coefs <- main_coefs |> filter(is.na(lexical_category))
  }
  collapsed <- main_coefs |>
    filter(as.character(term) %in% terms, measure %in% measures) |>
    mutate(coef_se = coef_se_from_hdi(conf.low, conf.high),
           corpus = childes_corpus(language)) |>
    filter(is.finite(coef_se), coef_se > 0, !is.na(corpus), corpus != "") |>
    group_by(corpus, measure, term) |>
    summarise(estimate = weighted.mean(estimate, 1 / coef_se^2),
              coef_se = sqrt(1 / sum(1 / coef_se^2)),
              .groups = "drop") |>
    left_join(
      morph_complexity |>
        mutate(corpus = childes_corpus(language)) |>
        group_by(corpus) |>
        summarise(morph_complexity = mean(morph_complexity), .groups = "drop"),
      by = "corpus"
    ) |>
    filter(!is.na(morph_complexity))

  grid <- expand_grid(measure = measures, ms_term = terms)
  fits <- map2(grid$measure, grid$ms_term, \(meas, trm) {
    brms::brm(
      estimate | se(coef_se, sigma = TRUE) ~ morph_complexity,
      data = collapsed |> filter(measure == meas, as.character(term) == trm),
      prior = brms::prior(student_t(3, 0, 2), class = "b"),
      iter = 2000,
      chains = 4,
      cores = 1,
      seed = 42,
      refresh = 0
    )
  })
  grid |> mutate(model = fits)
}

xgb_language_folds <- function(languages, k = 5) {
  langs <- unique(as.character(languages))
  k <- min(k, length(langs))
  fold_of <- setNames(sample(rep(seq_len(k), length.out = length(langs))), langs)
  assigned <- unname(fold_of[as.character(languages)])
  map(seq_len(k), \(i) which(assigned == i))
}

draw_xgb_params <- function(n) {
  tibble(
    learning_rate = 10^runif(n, log10(0.02), log10(0.3)),
    max_depth = sample(2:8, n, replace = TRUE),
    min_child_weight = 10^runif(n, log10(1), log10(16)),
    subsample = runif(n, 0.5, 1),
    colsample_bytree = runif(n, 0.5, 1),
    reg_lambda = 10^runif(n, log10(1e-2), log10(10))
  )
}

xgb_param_cols <- function() {
  c("learning_rate", "max_depth", "min_child_weight",
    "subsample", "colsample_bytree", "reg_lambda")
}

xgb_cv_rmse <- function(params, dmat, folds, nrounds) {
  params <- params |>
    select(all_of(xgb_param_cols())) |>
    as.list() |>
    lapply(unlist)
  fit <- xgb.cv(
    params = c(params, list(objective = "reg:squarederror", eval_metric = "rmse")),
    data = dmat,
    nrounds = nrounds,
    nfold = length(folds),
    folds = folds,
    verbose = 0
  )
  fit$evaluation_log$test_rmse_mean[[nrounds]]
}

hyperband_xgb <- function(dmat, folds, eta = 3, max_rounds = 270, min_rounds = 30) {
  s_max <- floor(log(max_rounds / min_rounds) / log(eta))
  param_cols <- xgb_param_cols()
  scored <- list()
  for (s in s_max:0) {
    n <- ceiling((s_max + 1) / (s + 1) * eta^s)
    r <- max_rounds / eta^s
    configs <- draw_xgb_params(n)
    for (i in 0:s) {
      r_i <- as.integer(round(r * eta^i))
      n_i <- nrow(configs)
      message(glue("Hyperband bracket {s}: {n_i} configs, {r_i} rounds"))
      rmse <- map_dbl(seq_len(n_i), \(j) {
        xgb_cv_rmse(configs[j, ], dmat, folds, r_i)
      })
      scored[[length(scored) + 1]] <- configs |>
        mutate(rmse = rmse, nrounds = r_i, bracket = s)
      if (i < s) {
        configs <- configs |>
          mutate(rmse = rmse) |>
          slice_min(rmse, n = max(1L, floor(n_i / eta)), with_ties = FALSE) |>
          select(all_of(param_cols))
      }
    }
  }
  list_rbind(scored)
}

