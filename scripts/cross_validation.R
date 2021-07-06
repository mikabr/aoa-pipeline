# require(modelr)

fit_cv_models <- function(word_values, formulae, loo_df = NULL) {
  if (is.null(loo_df)) { loo_df <- crossv_loo(ungroup(word_values)) }

  fit_cv_models_single <- function(id) {
    models <- "no model"
    train_idx <- loo_df[id,1][[1]][[1]]$idx
    test_idx <- loo_df[id,2][[1]][[1]]$idx
    train_df <- word_values[train_idx,]

    try(models <- fit_with(train_df, lm, formulae))

    result <- enframe(models) |>
      mutate(model = value,
             train = list(train_idx),
             test = list(test_idx)) |>
      select(-c(value))

    return(result)
  }

  loo_models <- loo_df$.id |>
    map(~ fit_cv_models_single(.)) |>
    reduce(rbind)
  return(loo_models)
}

get_cv_preds <- function(loo_models, word_values) {
  get_aoa_pred <- function(n){
    row <- tibble(
      name = loo_models$name[n],
      test = loo_models$test[n],
      train = loo_models$train[n],
      model = loo_models$model[n],
      test_word = word_values$uni_lemma[as.numeric(test)],
      lexical_category = word_values$lexical_category[as.numeric(test)],
      aoa = word_values$aoa[as.numeric(test)],
      aoa_pred = predict(model[[1]],  word_values[as.numeric(test),]))
    return(row)
  }

  loo_preds <- map(c(1:nrow(loo_models)), get_aoa_pred) |>
    bind_rows() |>
    mutate(abs_dev = abs(aoa - aoa_pred),
           se = abs_dev ^ 2)
  return(loo_preds)
}

get_cv_results <- function(loo_preds) {
  results <- loo_preds |>
    group_by(name) |>
    summarise(mean_abs_dev = mean(abs_dev),
              sd_abs_dev = sd(abs_dev),
              rmse = sqrt(mean(se)), mse = mean(se)) |>
    mutate(ci_mad = 1.96 * (sd_abs_dev / sqrt(n())),
      ci_mad_min = mean_abs_dev - ci_mad,
      ci_mad_max = mean_abs_dev + ci_mad)
  return(results)
}
