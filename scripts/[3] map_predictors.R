pred_dir <- "data/predictors"

# most predictors are attached to unilemmas/underlying concepts
# (e.g. concreteness) and can be generalized across languages
map_predictor <- function(uni_lemmas, predictor, variable_mapping) {

  # takes in a csv of predictor measures that may or may not map to
  # uni_lemmas and maps them
  predictor_data <- read_csv(glue("{pred_dir}/{predictor}.csv"))
  replacements <- read_csv(glue("{pred_dir}/{predictor}_replace.csv"))
  predictors <- discard(names(variable_mapping), \(v) v == "word")

  # clean the predictors
  renamed_predictors <- predictor_data |>
    rename(all_of(variable_mapping)) |>
    select(names(variable_mapping)) |>
    group_by(word) |>
    # in case a word appears in the list twice
    summarize(across({{ predictors }}, mean))

  # TODO: What do we do about things like "chips" and "can (auxiliary)"
  # chips doesnt match to "chip" and "can" gets the measures for "can (object)"
  uni_lemma_predictors <- uni_lemmas |> left_join(replacements) |>
    # if there is a hand written replacement, use the replacement
    # otherwise default to the cleaned uni_lemma
    mutate(word = case_when(
      !is.na(replacement) & replacement != "" ~ replacement,
      TRUE ~ str_replace(uni_lemma, "\\s*\\([^\\)]+\\)", ""))
    ) |>
    select(-replacement) |>
    left_join(renamed_predictors) |>
    select(-word) |>
    group_by(language, uni_lemma) |>
    summarize(across({{ predictors }}, mean)) |>
    ungroup()

  return(uni_lemma_predictors)
}
