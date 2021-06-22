source("scripts/stemmer.R")
childes_path <- "data/childes"

lang_map <- read_csv("resources/language_map.csv")

convert_lang_childes <- function(lang) {
  lang_map |> filter(wordbank == lang) |> pull(childes)
}

convert_lang_stemmer <- function(lang) {
  lang_map |> filter(wordbank == lang) |> pull(snowball)
}

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
    mutate(token_phonemes = if_else(token_phonemes == "", as.character(NA),
                                    token_phonemes),
    length_phon = as.double(str_length(token_phonemes))) |>
    group_by(token) |>
    summarise(length_phon = mean(length_phon, na.rm = TRUE)) |>
    mutate(length_phon = if_else(is.nan(length_phon), as.double(NA), length_phon))
}

default_metric_funs <- list(compute_count, compute_mlu, compute_positions,
                            compute_length_char, compute_length_phon)
default_corpus_args <- list(corpus = NULL, role = NULL,
                            role_exclude = "Target_Child", age = NULL,
                            sex = NULL, part_of_speech = NULL, token = "*")

get_childes_metrics <- function(lang, metric_funs = default_metric_funs,
                                corpus_args = default_corpus_args,
                                write = TRUE, half=NULL) {

  childes_lang <- convert_lang_childes(lang)

  if (!is.null(half)){
   childes_data <- half
  }else{
   childes_data <- get_childes_data(childes_lang, corpus_args)
  }
  utterances <- childes_data$utterances |>
    mutate(gloss = tolower(gloss)) |>
    select(utterance_id = id, utterance = gloss, utterance_length = num_tokens)

  tokens <- childes_data$tokens |>
    filter(gloss != "") |>
    mutate(gloss = tolower(gloss))|>
    select(token_id = id, token = gloss, token_order,
           token_phonemes = actual_phonology, utterance_id)

  metric_data <- tokens |> left_join(utterances)

  metrics <- map(metric_funs, \(fun) fun(metric_data)) |>
    reduce(partial(full_join, by = "token")) |>
    mutate(language = lang) |>
    mutate(count = count + 1,
           frequency=log(count/sum(count)),
           count_last=count_last +1,
           freq_last=log(count_last/sum(count_last)),
           count_first=count_first+1,
           freq_first=log(count_first/sum(count_first)),
           count_solo=count_solo+1,
           freq_solo=log(count_solo/sum(count_solo)))

  if (write) {
    norm_lang <- normalize_language(lang)
    metrics_file <- glue("{childes_path}/token_metrics_{norm_lang}.rds")
    saveRDS(metrics, metrics_file)
  }
  return(metrics)
}

get_childes_data <- function(childes_lang, corpus_args) {

  file_t <- file.path(childes_path, glue("{childes_lang}_tokens.rds"))
  file_u <- file.path(childes_path, glue("{childes_lang}_utterances.rds"))

  if(!file.exists(file_u)) {
  utterances <- get_utterances(language = childes_lang,
                               corpus = corpus_args$corpus,
                               role = corpus_args$role,
                               role_exclude = corpus_args$role_exclude,
                               age = corpus_args$age,
                               sex = corpus_args$sex)
  } else {
  utterances<-readRDS(file_u)
  }
  if(!file.exists(file_t)) {
  tokens <- get_tokens(language = childes_lang,
                       corpus = corpus_args$corpus,
                       role = corpus_args$role,
                       role_exclude = corpus_args$role_exclude,
                       age = corpus_args$age,
                       sex = corpus_args$sex,
                       token = corpus_args$token)
  } else {
    tokens<-readRDS(file_t)
  }
  return(list("utterances" = utterances, "tokens" = tokens))
}

transforms <- list(
  \(s) str_replace_all(s, "(.*) \\(.*\\)", "\\1"), # foo (bar) -> foo
  \(s) str_replace_all(s, " ", "_"), # foo bar -> foo_bar
  \(s) str_replace_all(s, " ", "+"), # foo bar -> foo+bar
  \(s) str_replace_all(s, "(.+) \\1", "\\1") # (foo) bar -> bar
  #   function(x) paste0(x, "e+moi"),
  #   function(x) paste0(x, "e-moi"),
  #   function(x) paste0(x, "+moi"),
  #   function(x) paste0(x, "-moi"),
  #   function(x) paste0(x, "ent"),
  #   function(x) paste0(x, "e-l"),
  #   function(x) paste0(x, "e-toi"),
  #   function(x) paste0(x, "-l"),
  #   function(x) paste0(x, "-toi"),
  #   function(x) paste0(x, "es-tu"),
  #   function(x) paste0(x, "s+tu"),
  #   function(x) paste0(x, "s-moi")
)

build_special_case_map <- function(language) {
  norm_lang <- normalize_language(language)
  special_case_file <- glue("resources/special_cases/{norm_lang}.csv")
  if (file.exists(special_case_file)) {
    read_csv(special_case_file, col_names = FALSE) |>
      rename(uni_lemma = X1, definition = X2) |>
      pivot_longer(-c(uni_lemma, definition),
                   names_to = "x", values_to = "option") |>
      filter(!is.na(option)) |>
      select(-x) |>
      mutate(language = language)
  }
}

build_options <- function(language, word, special_cases) {
  opts <- c(word, special_cases)
  opts <- c(opts, word |> str_split("[,/]") |> unlist()) # "foo, bar", "foo/bar"
  opts <- c(opts, map(transforms, \(t) t(opts)))
  opts <- opts |> unlist() |> unique() |> str_trim()
  stemmer_lang <- convert_lang_stemmer(language)
  if (!is.na(stemmer_lang)) opts <- c(opts, stem(opts, stemmer_lang))
  opts <- opts |> unique()
}

# construct a mapping from CDI items to various potential realizations of them
# in CHILDES
build_uni_lemma_map <- function(uni_lemmas) {
  special_case_map <- unique(uni_lemmas$language) |>
    map_df(build_special_case_map) |>
    group_by(language, uni_lemma, definition) |>
    summarise(special_cases = list(option))

  uni_lemmas |>
    unnest(items) |>
    left_join(special_case_map) |>
    mutate(option = pmap(list(language, definition, special_cases),
                         build_options)) |>
    select(language, uni_lemma, option) |>
    unnest(option)
}

get_uni_lemma_metrics <- function(lang, uni_lemmas) {
  uni_lemma_map <- build_uni_lemma_map(uni_lemmas)
  norm_lang <- normalize_language(lang)
  token_metrics_file <- glue("{childes_path}/token_metrics_{norm_lang}.rds")
  token_metrics <- readRDS(token_metrics_file)

  metrics_mapped <- token_metrics |>
    mutate(token_stem = stem(token, convert_lang_stemmer(lang))) |>
    inner_join(uni_lemma_map |> rename(token = option)) |>
    inner_join(uni_lemma_map |> rename(token_stem = option)) |>
    select(-language, -token_stem) |>
    rename(tokens = token) |>
    group_by(uni_lemma)

  metrics_summaries <- list(
    metrics_mapped |>
    summarise(across(where(is_character), \(col) list(unique(col)))),
    metrics_mapped |> summarise(across(starts_with("freq"), sum)),
    metrics_mapped |> summarise(across(where(is_integer), sum)),
    metrics_mapped |> summarise(across(where(is.double) & !starts_with("freq"), ~weighted.mean(., frequency)))
  )

  uni_metrics <- metrics_summaries |>
    reduce(partial(left_join, by = "uni_lemma")) |>
    mutate(n_tokens = map_int(tokens, length), language = lang) |>
    select(language, uni_lemma, tokens, n_tokens, everything())

  uni_metrics_file <- glue("{childes_path}/uni_metrics_{norm_lang}.rds")
  saveRDS(uni_metrics, uni_metrics_file)
}
