##...................................
## Data prep -- scaling, imputation, etc
##...................................

# drop predictors with all NAs

drop_predictors <- function(predictors, data) {
  # lapply(predictor_sources, \(y) {
  #   pred_source <- lapply(y, \(x) {if (!all(is.na(lang_data[x]))) x})
  #   pred_source[lengths(pred_source) > 0]
  # })
  predictors |> discard(\(p) all(is.na(data[[p]])))
}

# transform any column that starts with "count" by smoothing (add 1),
# normalizing, and log transforming, then rename "count_x" to "freq_x"

transform_counts <- function(childes_metrics, smooth = TRUE, normalize = TRUE,
                             log_transform = TRUE) {
  trans_metrics <- childes_metrics |> group_by(language)
  trans_funs <- c()
  if (smooth) trans_funs <- c(trans_funs, \(count) count + 1)
  if (normalize) trans_funs <- c(trans_funs, \(count) count / sum(count))
  if (log_transform) trans_funs <- c(trans_funs, \(count) log(count))

  #morph_funs <-c()
  #if (log_transform) morph_funs <- c(morph_funs, \(count) log(count))

  for (fun in trans_funs) {
    trans_metrics <- trans_metrics |> mutate(across(starts_with("count"), fun))
  }
  #for (fun in morph_funs) {
  #  trans_metrics <- trans_metrics |> mutate(across(starts_with("n_"), fun))
  #}
  trans_metrics |> ungroup() |>
    rename_with(\(col) str_replace(col, "count", "freq"), starts_with("count"))
}

residualize_col <- function(target_column, residualizing_column) {
  if (all(is.na(target_column))) return(NA)
  return(lm(target_column ~ residualizing_column)$residuals)
}

# residualize all columns that starts with "freq_" from the column "freq"
residualize_freqs <- function(childes_metrics) {
  childes_metrics |>
    mutate(across(starts_with("freq_"), partial(residualize_col, freq)))
}


residualize_morph <- function(lang, childes_metrics) {
  residualized <- childes_metrics |>
    filter(language == lang,
           !is.na(n_forms),
           !is.na(n_morph_categories),
           all(is.na(n_affixes)) | !is.na(n_affixes)) |>
    mutate(across(all_of(c("n_morph_categories", "n_affixes")),
           partial(residualize_col, n_forms))) |>
    select(uni_lemma, n_morph_categories, n_affixes)

  childes_metrics |>
    filter(language == lang) |>
    select(-c("n_morph_categories", "n_affixes")) |>
    left_join(residualized)

  # if (lang %in% l_with_n_affixes){
  #   message(glue(" {lang} containing data for n_affixes."))
  # a<-  childes_metrics |>
  #   filter(language == lang) |>
  #   filter(!is.na(n_forms)) |>
  #   filter(!is.na(n_affixes)) |>
  #   filter(!is.na(n_morph_categories)) |>
  #   mutate(across(starts_with("n_morph_categories"), partial(residualize_col, n_forms))) |>
  #   mutate(across(starts_with("n_affixes"), partial(residualize_col, n_forms)))
  # f <- childes_metrics |>
  #   filter(language == lang) |>
  #   select(-n_affixes, -n_morph_categories, -n_forms) |>
  #   left_join(a)
  # } else {
  #   message(glue("{lang} not containing data for n_affixes."))
  # a <- childes_metrics |>
  #   filter(language == lang) |>
  #   filter(!is.na(n_forms)) |>
  #   filter(!is.na(n_morph_categories))
  # if (!(all(is.na(a[,"n_morph_categories"])))) {
  #   a<- a|>
  #   mutate(across(starts_with("n_morph_categories"), partial(residualize_col, n_forms)))
  # }
  # f<- childes_metrics |>
  #  filter(language == lang) |>
  #  select(-n_morph_categories, -n_forms) |>
  #   left_join(a)
  # f$n_affixes = NA
  # }
  # return(f)
}

## Imputation
fit_predictor <- function(pred, d, pred_sources) {
  xs <- pred_sources |> discard(\(s) pred %in% s) |> unlist()
  x_str <- xs |> paste(collapse = " + ")
  lm(as.formula(glue("{pred} ~ {x_str}")), data = d) |>
    broom::augment(newdata = d) |>
    select(uni_lemma, category, lexical_category, .fitted)
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
  # iterates through the predictor list for that language
  for (pred in prediction_list) {

    imputation_fits <- fit_predictor(pred, imputation_data, pred_sources)
    imputation_data <- missing |>
      select(uni_lemma, lexical_category, category, !!pred) |>
      rename(missing = !!pred) |>
      right_join(imputation_data,
                 by = c("uni_lemma", "lexical_category", "category")) |>
      left_join(imputation_fits,
                by = c("uni_lemma", "lexical_category", "category")) |>
      # if the value is missing, replace it with the new value
      mutate_at(vars(pred), funs(if_else(is.na(missing), .fitted, .))) |>
      select(-.fitted, -missing)
  }
  return(imputation_data)
}

do_lang_imputation <- function(language, data, pred_sources, max_steps) {
  pred_sources <- lapply(pred_sources, \(x) drop_predictors(x, data))
  # if all the predictors are from one source, fix the pred_sources
  if (length(pred_sources) == 1) pred_sources <- unlist(pred_sources)
  data <- data |> distinct()
  predictors <- unlist(pred_sources)

  print(glue("Imputing {language} with {max_steps} steps..."))
  predictor_list <- get_predictor_order(data, predictors, max_steps)
  # print(glue("get predictor order."))
  missing_data <- get_missing_data(data, predictors)
  # print(glue("get missing data"))
  imputed_data <- get_imputation_seed(data, predictors)
  # print(glue("get imputation seed"))
  # print(glue("pred_sources"))
  # print(glue("{pred_sources}"))
  imputed_data <- do_iterate_imputation(pred_sources, imputed_data,
                                        missing_data)
  # print(glue("do iterate imputation"))
  scaled_data <- do_scaling(imputed_data, predictors)
  # print(glue("do scaling"))
  return(imputed_data)
}

do_full_imputation <- function(model_data, predictor_sources, max_steps) {
  # restrict to the sources in pred_sources
  # catch cases where a predictor in the predictor set isn't in the data

  #language = model_data$language[1][1]
  #l = normalize_language(language)
  #file__ <-  glue("{childes_path}/imputed_scaled_{l}.rds")
  #if (file.exists(file__)) {
  #  imputed_data <- readRDS(file__)
  #}else{

  nested_data <- model_data |>
    select(language, uni_lemma, lexical_category, category,
           all_of(!!unlist(predictor_sources))) |>
    distinct() |>
    group_by(language) |>
    nest()

  imputed_data <- nested_data |>
    mutate(imputed = purrr::map2(language, data, function(lang, dat) {
      do_lang_imputation(lang, dat, predictor_sources, max_steps)
    }))

  imputed_data <- imputed_data |>
    ungroup() |>
    select(language, imputed) |>
    unnest(imputed) |>
    distinct()
#  saveRDS(imputed_data, file__)
 # }
  return(imputed_data)
}


# scaling predictors
do_scaling <- function(model_data, predictors) {
  model_data |>
    #group_by(language) |>
    mutate(across(all_of(predictors), \(x) as.numeric(scale(x))))
}


prep_lexcat <- function(predictor_data, uni_lemmas, ref_cat) {
  lexical_categories <- uni_lemmas |>
    unnest(items) |>
    distinct() |>
    # mutate(lexical_category = sapply(items, \(x) {
    #   lc <- x |> pull(lexical_category) |> unique()
    #   if(length(lc) > 1) return("other") else lc
    # })) |>
    # uni_lemmas with item in multiple different classes treated as "other"
    filter(!lexical_category=="other") |>
           # collapse v, adj, adv into one category
    mutate(lexical_category = lexical_category |> as_factor() |>
             # fct_collapse("predicates" = c("verbs", "adjectives")) |>
             fct_relevel("nouns","predicates","function_words") |>
             fct_relevel(ref_cat))

  contrasts(lexical_categories$lexical_category) <- contr.sum

  predictor_data |>
    left_join(lexical_categories) |>
    filter(!is.na(uni_lemma))  |>
    filter(!is.na(lexical_category)) |>
    distinct()
}
