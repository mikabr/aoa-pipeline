library(data.table)

compute_count <- function(metric_data) {
  print("Computing count...")
  metric_data |> count(token, name = "count")
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

compute_form_entropy <- function(metric_data) {
  print("Computing form entropy...")
  metric_data |>
    select(token, token_stem) |>
    nest(tokens = token) |>
    mutate(form_entropy = sapply(tokens, \(t) {
      t |>
        table() |>
        as_tibble() |>
        mutate(freq = n / sum(n),
               ent = freq * log2(freq)) |>
        pull(ent) |>
        sum() |>
        (`*`)(-1)
    })) |>
    select(-token_stem) |>
    unnest(tokens) |>
    distinct()
}

default_metric_funs <- list(compute_count, compute_mlu, compute_positions,
                            compute_length_char, compute_length_phon,
                            compute_n_cats, compute_n_forms, compute_verb_frames)
default_corpus_args <- list(corpus = NULL, role = NULL,
                            role_exclude = "Target_Child", age = NULL,
                            sex = NULL, part_of_speech = NULL, token = "*")

get_childes_data <- function(childes_lang, corpus_args) {

  file_t <- file.path(childes_path, glue("tokens_{childes_lang}.rds"))
  file_u <- file.path(childes_path, glue("utterances_{childes_lang}.rds"))

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
  return(list("utterances" = utterances, "tokens" = tokens))
}


get_token_metrics <- function(lang, metric_funs = default_metric_funs,
                              corpus_args = default_corpus_args,
                              write = TRUE, import_data = NULL,
                              use_morphology = TRUE) {

  childes_lang <- convert_lang_childes(lang)
  print({childes_lang})
  if (length(childes_lang) == 0)
    message(glue("Language {lang} not found in CHILDES"))

  if (!is.null(import_data)) {
    childes_data <- import_data
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args)
  }

  utterances <- childes_data$utterances |>
    mutate(gloss = tolower(gloss)) |>
    select(utterance_id = id, utterance = gloss, utterance_length = num_tokens)

  tokens <- childes_data$tokens |>
    filter(gloss != "") |>
    mutate(gloss = tolower(gloss), stem = tolower(stem))|>
    select(token_id = id, token = gloss, token_stem = stem, token_order,
           token_phonemes = actual_phonology, utterance_id, language)
 # token_stems <- tokens |> select(token, token_stem) |> distinct()

  if (use_morphology & childes_lang != "zho") {
    morph_data <- load_morph_data(lang, corpus_args)
    tokens <- tokens |> left_join(morph_data,
                                  by = c("utterance_id", "token_id" = "id"))
  }

  metric_data <- tokens |> left_join(utterances)
  print("Calculating token_metrics")
  token_metrics <- map(metric_funs, \(fun) fun(metric_data)) |>
    reduce(partial(full_join, by = c("token"))) |>
    mutate(language = lang) |>
    mutate(freq_raw = count / sum(count))
  # across(starts_with("count"), sum, .names = "sum{.col}"))

  if (write) {
    norm_lang <- normalize_language(lang)
    metrics_file <- glue("{childes_path}/token_metrics_{norm_lang}.rds")
    saveRDS(token_metrics, metrics_file)
  }
  return(token_metrics)
}

transforms <- list(
  \(s) str_replace_all(s, "(.*) \\(.*\\)", "\\1"), # foo (bar) -> foo
  \(s) str_replace_all(s, " ", "_"), # foo bar -> foo_bar
  \(s) str_replace_all(s, " ", "+"), # foo bar -> foo+bar
  \(s) str_replace_all(s, "(.+) \\1", "\\1") # (foo) bar -> bar
)

build_special_case_map <- function(lang) {
  norm_lang <- normalize_language(lang)
  special_case_file <- glue("resources/special_cases/{norm_lang}.csv")
  if (file.exists(special_case_file)) {
    special_case_map <- read_csv(special_case_file, col_names = FALSE) |>
      rename(uni_lemma = X1, item_definition = X2) |>
      pivot_longer(-c(uni_lemma, item_definition),
                   names_to = "x", values_to = "option") |>
      filter(!is.na(option)) |>
      select(-x) |>
      mutate(language = lang)
  } else {
    special_case_map <- tibble(language = character(), uni_lemma = character(),
                               item_definition = character(), option = character())
  }
  return(special_case_map)
}

build_options <- function(language, word, special_cases) {
  opts <- c(word, special_cases)
  opts <- c(opts, word |> str_split("[,/]") |> unlist()) # "foo, bar", "foo/bar"
  opts <- c(opts, map(transforms, \(t) t(opts)))
  opts <- opts |> unlist() |> unique() |> str_trim()
  opts <- c(opts, stem(opts, language))
  return(unique(opts))
}

# construct a mapping from CDI items to various potential realizations of them
# in CHILDES
build_uni_lemma_map <- function(uni_lemmas) {
  special_case_map <- unique(uni_lemmas$language) |>
    map_df(build_special_case_map) |>
    group_by(language, uni_lemma, item_definition) |>
    summarise(special_cases = list(option))
  uni_lemmas |>
    unnest(items) |>
    left_join(special_case_map) |>
    mutate(option = pmap(list(language, item_definition, special_cases),
                         build_options)) |>
    select(language, uni_lemma, option) |>
    unnest(option) |>
    mutate(option = tolower(option))
}

get_uni_lemma_metrics <- function(lang, uni_lemma_map, import_data = NULL) {
  norm_lang <- normalize_language(lang)
  print({norm_lang})
  if (!is.null(import_data)) {
    token_metrics <- import_data
  } else {
    token_metrics_file <- glue("{childes_path}/token_metrics_{norm_lang}.rds")
    if (!file.exists(token_metrics_file)) {
      message(glue("No cached token metrics for {lang}, getting and caching data."))
      # note: this gets token metrics with default args
      token_metrics <- get_token_metrics(lang)
    } else {
      token_metrics <- readRDS(token_metrics_file)
    }
  }
  tokens_mapped <- token_metrics |>
    select(token) |>
    mutate(token_self = token,
           token_stemmed = stem(token, lang)) |>
    pivot_longer(c(token_self, token_stemmed), names_to = "src",
                 values_to = "option") |>
    filter(!is.na(option), option != "") |>
    select(-src) |>
    distinct() |>
    inner_join(uni_lemma_map) |>
    group_by(uni_lemma, token) |>
    summarise(options = list(option)) |>
    ungroup()

  metrics_mapped <- tokens_mapped |>
    inner_join(token_metrics) |>
    select(uni_lemma, tokens = token, where(is.numeric)) |>
    group_by(uni_lemma) |>
    distinct() |>
    filter(!is.na(count))

  uni_lemma_tokens <- tokens_mapped |>
    select(uni_lemma, token) |>
    nest(options = c(token))# removed stem from realizations n_types

  metrics_summaries <- list(
    metrics_mapped |>
      summarise(across(where(is_character), \(col) list(unique(col)))),
    metrics_mapped |>
      summarise(across(where(is_integer), sum)),
    metrics_mapped |>
      summarise(across(where(is.double) & !starts_with("freq"),
                       \(x) weighted.mean(x, count, na.rm = TRUE)))
  )

  uni_metrics <- metrics_summaries |>
    reduce(partial(left_join, by = "uni_lemma")) |>
    inner_join(uni_lemma_tokens) |>
    mutate(n_types_old = map_int(tokens, length), language = lang)

  uni_metrics_file <- glue("{childes_path}/uni_metrics_{norm_lang}.rds")
  saveRDS(uni_metrics, uni_metrics_file)
  return(uni_metrics)
}

load_childes_metrics <- function(languages, uni_lemmas, cache = TRUE) {
  uni_metrics <- map_df(languages, function(lang) {
    norm_lang <- normalize_language(lang)
    lang_file <- glue("{childes_path}/uni_metrics_{norm_lang}.rds")
    if (file.exists(lang_file)) {
      message(glue("Loading cached CHILDES metrics for {lang}..."))
      lang_metrics <- readRDS(lang_file)
      lang_metrics <- tryCatch({lang_metrics <- lang_metrics %>%
        rename(n_types_old = n_tokens)
      }, error= function(e){lang_metrics})
    } else {
      if (cache) {
        message(glue("No cached CHILDES metrics for {lang}, getting and caching data."))
        uni_lemma_map <- build_uni_lemma_map(uni_lemmas |> filter(language == lang))
        lang_metrics <- get_uni_lemma_metrics(lang, uni_lemma_map)
      } else {
        message(glue("No cached CHILDES metrics for {lang}, skipping."))
        lang_metrics <- tibble()
      }
    }
    return(lang_metrics)
  })
  return(uni_metrics)
}
