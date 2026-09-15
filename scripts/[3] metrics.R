default_metric_funs <- list(base = list(compute_count, compute_transcript_diversity,
                                        compute_mlu, compute_positions,
                                        compute_length_char, compute_length_phon,
                                        compute_burstiness),
                            parsed = list(compute_form_entropy, compute_subcat_entropy,
                                          compute_mdd, compute_n_features,
                                          compute_contextual_diversity),
                            morph = list(compute_n_morphemes)) #,
                            # embed = list(compute_semantic_diversity,
                            #              compute_semantic_support))

run_metric_funs <- function(funs, data) {
  map(funs, \(fun) {
    out <- fun(data)
    gc()
    out
  })
}

get_token_metrics <- function(lang, metric_funs = default_metric_funs,
                              corpus_args = default_corpus_args,
                              write = TRUE, import_data = NULL,
                              overwrite = FALSE) {

  childes_lang <- convert_lang_childes(lang)
  # print({childes_lang})
  if (length(childes_lang) == 0)
    message(glue("Language {lang} not found in CHILDES"))

  metrics_file <- glue("{childes_path}/token_metrics_{childes_lang}.rds")
  # Dialects (e.g. the three English CDIs) share one CHILDES language; reuse
  # the token-level cache instead of holding several copies of eng in memory.
  if (is.null(import_data) && !overwrite && file.exists(metrics_file)) {
    message(glue("Loading cached token metrics for {lang} ",
                 "(CHILDES '{childes_lang}')..."))
    return(readRDS(metrics_file) |> mutate(language = lang))
  }

  if (!is.null(import_data)) {
    childes_data <- import_data
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args)
  }

  metric_parts <- list()

  if (!is.null(metric_funs$base)) {
    # Base metrics only need utterance *length*, not the gloss string (which
    # would be duplicated onto every token and dominate RAM for English).
    utterances <- childes_data$utterances |>
      select(utterance_id = id, utterance_length = num_tokens)
    tokens <- childes_data$tokens |>
      filter(gloss != "") |>
      mutate(gloss = tolower(gloss)) |>
      select(token = gloss, token_order, token_phonemes = actual_phonology,
             utterance_id, transcript_id)
    base_data <- tokens |> left_join(utterances, by = "utterance_id")
    rm(utterances, tokens)
    gc()

    print("Calculating token_metrics")
    metric_parts <- c(metric_parts, run_metric_funs(metric_funs$base, base_data))
    rm(base_data)
    gc()
  } else {
    print("Calculating token_metrics")
  }

  # Drop the raw CHILDES tables before loading parsed/morph (each is multi-GB
  # for English). They are re-read from RDS if a later step still needs them.
  rm(childes_data)
  gc()

  if (!is.null(metric_funs$parsed)) {
    metric_parts <- c(
      metric_parts,
      compute_parsed_metrics_from_chunks(lang, metric_funs$parsed, corpus_args)
    )
    gc()
  }

  if (!is.null(metric_funs$morph)) {
    morph <- load_morph_data(lang, corpus_args)
    metric_parts <- c(metric_parts, run_metric_funs(metric_funs$morph, morph))
    rm(morph)
    gc()
  }

  if (!is.null(metric_funs$embed)) {
    childes_data <- if (!is.null(import_data)) {
      import_data
    } else {
      get_childes_data(childes_lang, corpus_args)
    }
    utterances <- childes_data$utterances |>
      mutate(gloss = tolower(gloss)) |>
      select(utterance_id = id, utterance = gloss)
    tokens <- childes_data$tokens |>
      filter(gloss != "") |>
      mutate(gloss = tolower(gloss)) |>
      select(token = gloss, token_order, utterance_id)
    rm(childes_data)
    parsed_embed <- load_parsed_data(lang, corpus_args = corpus_args)
    utt_embeds <- load_utt_embed_data(lang, corpus_args) |>
      asplit(MARGIN = 1)
    lemma_embeds <- load_lemma_embed_data(lang, corpus_args) |>
      # this is slow---probably could be faster?
      rowwise() |>
      mutate(lemma_embed = c_across(-lemma) |> list()) |>
      select(lemma, lemma_embed)
    embed_data <- tokens |>
      left_join(utterances |> mutate(utterance_embed = utt_embeds),
                by = join_by(utterance_id)) |>
      left_join(parsed_embed |>
                  select(utterance = sentence, token_order = token_id, token, lemma) |>
                  mutate(token_order = as.integer(token_order)) |>
                  distinct(),
                by = join_by(utterance, token_order, token)) |>
      left_join(lemma_embeds, by = join_by(lemma))
    rm(utterances, tokens, parsed_embed, utt_embeds, lemma_embeds)
    gc()
    metric_parts <- c(metric_parts, run_metric_funs(metric_funs$embed, embed_data))
    rm(embed_data)
    gc()
  }

  token_metrics <- metric_parts |>
    reduce(partial(left_join, by = "token", relationship = "one-to-one")) |>
    mutate(language = lang)

  if (write) {
    saveRDS(token_metrics, metrics_file)
  }
  return(token_metrics)
}

transforms <- list(
  \(s) str_replace_all(s, "(.*) ?[\\(（].*[\\)）]", "\\1"), # foo (bar) -> foo
  \(s) str_replace_all(s, "[\\(（].*[\\)）] ?(.+)", "\\1"), # (foo) bar -> bar
  \(s) str_replace_all(s, " ", "_"), # foo bar -> foo_bar
  \(s) str_replace_all(s, " ", "+"), # foo bar -> foo+bar
  \(s) str_replace_all(s, "(.*)[\\(（](.*)[\\)）]", "\\1\\2"), # foo(bar) -> foobar
  \(s) str_replace_all(s, "[\\(（](.*)[\\)）](.*)", "\\1\\2") # (foo)bar -> foobar
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

build_options <- function(lang, word, uni_lemma, special_cases) {
  opts <- c(word, special_cases)
  opts <- c(opts, word |> str_split("[,/、]") |> unlist()) # "foo, bar", "foo/bar"
  opts <- c(opts, map(transforms, \(t) t(opts)))
  opts <- opts |> unlist() |> unique() |> str_trim()
  if (!str_detect(lang, "French \\(") || !str_detect(uni_lemma, "[0-9]")) { # hacky fix for French pronouns
    opts <- c(opts, lemmatize(opts, lang))
  }
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
    left_join(special_case_map,
              by = c("language", "uni_lemma", "item_definition")) |>
    mutate(option = pmap(list(language, item_definition, uni_lemma, special_cases),
                         build_options)) |>
    select(language, uni_lemma, option) |>
    unnest(option) |>
    mutate(option = tolower(option),
           option = ifelse(language == "Mandarin (Taiwanese)",
                           option |> tmcn::toTrad(rev = TRUE),
                           option))
}

get_uni_lemma_metrics <- function(lang, uni_lemma_map, import_data = NULL,
                                  metric_funs = default_metric_funs) {
  childes_lang <- convert_lang_childes(lang)
  norm_lang <- normalize_language(lang)
  # print({norm_lang})
  if (!is.null(import_data)) {
    token_metrics <- import_data
  } else {
    token_metrics_file <- glue("{childes_path}/token_metrics_{childes_lang}.rds")
    if (!file.exists(token_metrics_file)) {
      message(glue("No cached token metrics for {lang}, getting and caching data."))
      token_metrics <- get_token_metrics(lang, metric_funs = metric_funs)
    } else {
      token_metrics <- readRDS(token_metrics_file)
    }
  }
  tokens_mapped <- token_metrics |>
    filter(token != "") |> # bad edge case in English (American)
    select(token) |>
    mutate(token_self = token,
           token_stemmed = lemmatize(token, lang)) |>
    pivot_longer(c(token_self, token_stemmed), names_to = "src",
                 values_to = "option") |>
    filter(!is.na(option), option != "") |>
    select(-src) |>
    distinct() |>
    inner_join(uni_lemma_map, by = "option", relationship = "many-to-many") |>
    group_by(uni_lemma, token) |>
    summarise(options = list(option)) |>
    ungroup()

  metrics_mapped <- tokens_mapped |>
    inner_join(token_metrics, by = "token") |>
    select(uni_lemma, tokens = token, where(is.numeric)) |>
    group_by(uni_lemma) |>
    distinct() |>
    filter(!is.na(count))

  uni_lemma_tokens <- tokens_mapped |>
    select(uni_lemma, token) |>
    nest(options = c(token))

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
    inner_join(uni_lemma_tokens, by = "uni_lemma") |>
    mutate(n_types_old = map_int(tokens, length), language = lang)

  uni_metrics_file <- glue("{childes_path}/uni_metrics_{norm_lang}.rds")
  saveRDS(uni_metrics, uni_metrics_file)
  return(uni_metrics)
}

load_childes_metrics <- function(langs, uni_lemmas, cache = TRUE,
                                 metric_funs = default_metric_funs) {
  uni_metrics <- map_df(langs, function(lang) {
    norm_lang <- normalize_language(lang)
    lang_file <- glue("{childes_path}/uni_metrics_{norm_lang}.rds")
    if (file.exists(lang_file)) {
      message(glue("Loading cached CHILDES metrics for {lang}..."))
      lang_metrics <- readRDS(lang_file)
      lang_metrics <- tryCatch({lang_metrics <- lang_metrics %>%
        rename(n_types_old = n_tokens)
      }, error = function(e){lang_metrics})
    } else {
      if (cache) {
        message(glue("No cached CHILDES metrics for {lang}, getting and caching data."))
        uni_lemma_map <- build_uni_lemma_map(uni_lemmas |> filter(language == lang))
        lang_metrics <- get_uni_lemma_metrics(lang, uni_lemma_map,
                                             metric_funs = metric_funs)
      } else {
        message(glue("No cached CHILDES metrics for {lang}, skipping."))
        lang_metrics <- tibble()
      }
    }
    return(lang_metrics)
  })
  return(uni_metrics)
}
