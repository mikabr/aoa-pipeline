##...................................
## data prep -- scaling, imputation, etc
##...................................

# HELPER FUNCTIONS --------------


#Normalize frequency: log transform and smoothing (+1)
normalize_frequency<- function(uni_childes) {
  uni_childes |>
  mutate(count = count + 1,
         frequency=log(count/sum(count)),
         count_last=count_last +1,
         final_frequency=log(count_last/sum(count_last)),
         count_first=count_first+1,
         first_frequency=log(count_first/sum(count_first)),
         count_solo=count_solo+1,
         solo_frequency=log(count_solo/sum(count_solo)))
}



# residualize - do each of these get their own functions? w/ an argument
get_final_freq <- function(uni_childes) {
  return(lm(final_freq ~ requency, data = uni_childes)$residuals)
}
get_solo_freq <- function(uni_childes) {
  return(lm(final_freq ~ requency, data = uni_childes)$residuals)
}


fit_predictor <- function(pred, d, pred_sources) {
  xs <- pred_sources |> discard(\(s) pred %in% s) |> unlist()
  x_str <- xs |> paste(collapse = " + ")
  lm(as.formula(glue("{pred} ~ {x_str}")), data = d) |>
    broom::augment(newdata = d) |>
    select(uni_lemma, lexical_category, .fitted)
}

get_missing_data <- function(lang_data, predictors) {
  missing <- lang_data |>
    pivot_longer(cols = !!predictors, names_to = "predictor",
                 values_to = "value") |>
    mutate(missing = is.na(value)) |>
    select(-value) |>
    pivot_wider(names_from = predictor, values_from = missing)
  return(missing)
}

# start with predictors with the fewest NAs
# create a list that can be iterated over
get_predictor_order <- function(lang_data, predictors, max_steps) {
  predictor_order <- lang_data |>
    pivot_longer(cols = !!predictors, names_to = "predictor",
                 values_to = "value") |>
    group_by(predictor) |>
    summarise(num_na = sum(is.na(value))) |>
    filter(num_na != 0) |>
    arrange(num_na) |>
    pull(predictor)

  num_repeats <- max_steps %/% length(predictor_order) + 1
  return(rep(predictor_order, num_repeats)[0:max_steps])
}

get_imputation_seed <- function(lang_data, predictors) {
  imputation_data <- lang_data |>
    mutate_at(vars(!!predictors),
              funs(as.numeric(Hmisc::impute(., fun = "random"))))
  return(imputation_data)
}

# takes in a predictor from the list of predictors, pulls the imputation data at
# current status, returns a new imputation data

do_iterate_imputation <- function(pred_sources, imputation_data, missing) {
  prediction_list <- unlist(pred_sources)
  #iterates through the predictor list for that language
  for (pred in prediction_list) {
    imputation_fits <- fit_predictor(pred, imputation_data, pred_sources)
    imputation_data <- missing |>
      select(uni_lemma, lexical_category, !!pred) |>
      rename(missing = !!pred) |>
      right_join(imputation_data) |>
      left_join(imputation_fits) |>
      #if the value is missing, replace it with the new value
      mutate_at(vars(pred), funs(if_else(is.na(missing), .fitted, .))) |>
      select(-.fitted, -missing)
  }
  return(imputation_data)
}


do_lang_imputation <- function(language, data, pred_sources, max_steps) {
  # if all the predictors are from one source, fix the pred_sources
  if (length(pred_sources) == 1) pred_sources <- unlist(pred_sources)
  predictors <- unlist(pred_sources)
  print(glue("Imputing {language} with {max_steps} steps..."))
  predictor_list <- get_predictor_order(data, predictors, max_steps)
  # what do we do if a language is missing a predictor entirely?
  missing_data <- get_missing_data(data, predictors)
  imputed_data <- get_imputation_seed(data, predictors)
  imputed_data <- do_iterate_imputation(pred_sources, imputed_data,
                                        missing_data)
  return(imputed_data)
}

do_full_imputation <- function(model_data, pred_sources, max_steps) {
  # restrict to the sources in pred_sources
  # TODO: catch cases where a predictor in the predictor set isn't in the data
  nested_data <- model_data |>
    select(language, uni_lemma, lexical_category, !!unlist(pred_sources)) |>
    distinct() |>
    group_by(language) |>
    nest()

  imputed_data <- nested_data |>
    mutate(imputed = map2(language, data,
                          \(lang, dat) do_lang_imputation(lang, dat,
                                                          pred_sources,
                                                          max_steps)))
  return(imputed_data)
}
