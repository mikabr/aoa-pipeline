default_corpus_args <- list(corpus = NULL, role = NULL,
                            role_exclude = "Target_Child", age = NULL,
                            sex = NULL, part_of_speech = NULL, token = "*")

get_childes_data <- function(childes_lang, corpus_args) {
  if (childes_lang == "jpn") return(get_childes_data_jpn(corpus_args))
  if (childes_lang == "ara") return(get_childes_data_ara(corpus_args))

  file_t <- here(childes_path, glue("tokens_{childes_lang}.rds"))
  file_u <- here(childes_path, glue("utterances_{childes_lang}.rds"))

  if (file.exists(file_u)) {
    utterances <- readRDS(file_u)
  } else {
    print("Getting CHILDES utterances")
    utterances <- get_utterances(language = childes_lang,
                                 corpus = corpus_args$corpus,
                                 role = corpus_args$role,
                                 role_exclude = corpus_args$role_exclude,
                                 age = corpus_args$age,
                                 sex = corpus_args$sex)
    saveRDS(utterances, file_u)
  }

  if (file.exists(file_t)) {
    tokens <- readRDS(file_t)
  } else {
    print("Getting CHILDES tokens")
    tokens <- get_tokens(language = childes_lang,
                         corpus = corpus_args$corpus,
                         role = corpus_args$role,
                         role_exclude = corpus_args$role_exclude,
                         age = corpus_args$age,
                         sex = corpus_args$sex,
                         token = corpus_args$token)
    saveRDS(tokens, file_t)

  }

  childes_data <- list("utterances" = utterances, "tokens" = tokens)

  if (childes_lang %in% c("rus", "heb", "ara") &&
      !file.exists(here(childes_path, glue("tokens_{childes_lang}_orig.rds")))) {
    childes_data <- process_childes(childes_data, childes_lang)
  }

  return(childes_data)
}

compute_count <- function(metric_data) {
  print("Computing count...")
  metric_data |> count(token, name = "count")
}

compute_cd <- function(metric_data) {
  print("Computing contextual diversity...")
  metric_data |> group_by(token) |> summarise(cd = n_distinct(transcript_id))
}

compute_mlu <- function(metric_data) {
  print("Computing mean utterance length...")
  metric_data |> group_by(token) |> summarise(mlu = mean(utterance_length))
}

compute_positions <- function(metric_data) {
  print("Computing utterance position counts...")
  metric_data |>
    mutate(order_first = token_order == 1,
           order_last = token_order == utterance_length,
           order_solo = utterance_length == 1) |>
    ungroup() |>
    select(token, starts_with("order")) |>
    group_by(token) |>
    summarise(across(everything(), sum)) |>
    mutate(order_first = order_first - order_solo,
           order_last = order_last - order_solo) |>
    rename_with(\(s) str_replace(s, "order", "count"), -token)
}

compute_length_char <- function(metric_data) {
  print("Computing length in characters...")
  metric_data |> distinct(token) |>
    mutate(length_char = as.double(str_length(token)))
}

compute_length_phon <- function(metric_data) {
  print("Computing length in phonemes...")
  metric_data |>
    distinct(token, token_phonemes) |>
    filter(token_phonemes != "") |>
    mutate(length_phon = as.double(str_length(token_phonemes))) |>
    group_by(token) |>
    summarise(length_phon = mean(length_phon, na.rm = TRUE),
              token_phonemes = list(token_phonemes))
}
