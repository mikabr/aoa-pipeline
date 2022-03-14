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


compute_verb_frame_df <- function(metric_data){

  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_)
  morph <- morph %>% mutate(next_pos = NA, next_pos2=NA, next_pos3=NA)
  morph$next_pos[morph$utterance_id == lead(morph$utterance_id, n=1, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph$next_pos2[morph$utterance_id == lead(morph$utterance_id, n=2, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph$next_pos3[morph$utterance_id == lead(morph$utterance_id, n=3, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph <- morph %>%
    mutate(next_pos = ifelse(next_pos=="same_utt",lead(pos, n=1, default=NA), NA ),
           next_pos2 = ifelse(next_pos2=="same_utt",lead(pos, n=2, default=NA), NA ),
           next_pos3 = ifelse(next_pos3=="same_utt",lead(pos, n=3, default=NA), NA ),
           )
  morph= morph %>% mutate(next_pos = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int", next_pos, ""),
                          next_pos2 = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int", next_pos2, ""),
                          next_pos3 = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int", next_pos3, ""))

  morph$next_pos <- paste(morph$next_pos,morph$next_pos2,morph$next_pos3,sep="_")
  morph <- morph %>% select(-next_pos2, - next_pos3)

  np <- c('^neg_n:prop', '^pro:dem_', '^n:prop', '^pro_pro:dem','^det:art_','^det:gen_n', '^co_NA','^n_n:prop','^det:dem_pro:rel', '^det:poss_n', '^det:art_adj', '^pro:dem_adv')
  pp <- c("^prep_det:art", "^pro:y_n:prop", "^prep_n", "^adv_prep")
  s <- c("^v_prop:dem", "^pro:obj_v", "^adv_part", "^v_prep", "^v:mdl_", "^v_NA", "^part_v:mdl", "^v_co_", "v_pro")
  adv_empty <- c("^adv:place", "^adv_pro:dem", "^adv_conj_")

  morph$frame <- NA
  morph$frame <- ifelse(grepl(paste(np, collapse = "|"), morph$next_pos),'NP',NA)
  morph$frame <- ifelse(grepl(paste(pp, collapse = "|"), morph$next_pos),'PP', morph$frame)
  morph$frame <- ifelse(grepl(paste(s, collapse = "|"), morph$next_pos),'S',morph$frame)
  morph$frame <- ifelse(grepl(paste(adv_empty, collapse = "|"), morph$next_pos),'ADV_EMPTY',morph$frame)

  morph |>
    filter(!is.na(frame))
}


compute_verb_frame <- function(metric_data){
  print("Computing verb frames")
  file_u <- file.path(childes_path, glue("verb_frames_{childes_lang}.rds"))
  if (file.exists(file_u)) {
    morph <- readRDS(file_u)
  } else {
    morph<-compute_verb_frame_df(metric_data)
    saveRDS(morph, file_u)
  }
  tmp_data <- metric_data |>
    select(token_id, token, utterance_id) |>
    left_join(morph)
  m <- tmp_data |>
    filter(!is.na(frame)) |>
    group_by(token, pos) |>
    summarise(c=n())
  final <- tmp_data |>
    filter(!is.na(frame)) |>
    group_by(token, frame, pos) |>
    summarise(tmp=n()) |>
    left_join(m) |>
    mutate(per= 100 *tmp/c)
  a <- final %>% group_by(token, pos) %>% filter(per == max(per)) %>%
    select(token, frame) %>% distinct() %>% rename(main_frame=frame)
  final |>
    group_by(token, pos) |>
    summarise(n_distinct_frame = n_distinct(frame), per_frame = max(per)) |>
    left_join(a[!duplicated(a$token), ]) |>
    group_by(token) |>
    summarise(n_distinct_frame = mean(n_distinct_frame, na.rm=TRUE),
              per_frame = mean(per_frame,  na.rm=TRUE),
              main_frame = paste(main_frame, collapse=","),)
}
}


compute_n_sfx_cat <-function(metric_data){
  print("Computing number of morphemes and categories")
  file_u <- file.path(childes_path, glue("n_sfx_cat_{childes_lang}.rds"))
  if (file.exists(file_u)) {
    morph <- readRDS(file_u)
  } else {
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_)
  morph$number.of.sfx <- str_remove(morph$affix_type_m, "sfxf")
  morph$number.of.sfx <- str_count(morph$number.of.sfx, "sfx")
  morph_ <- metric_data |>
    left_join(morph) |>
    mutate(n_affix = ifelse(affix_m=="NULL", NA, lengths(as.list(affix_m)))) |>
    group_by(token, pos) |>
    summarise(n_category = mean(n_affix, na.rm = TRUE),
              token_cats = list(unique(na.omit(affix_m))),
              n_sfx = mean(number.of.sfx, na.rm = TRUE)) |>
    filter(!is.na(n_category)) |>
    group_by(token) |>
    summarise(token_morphemes = paste(token_morphemes, collapse=","),
              n_sfx = mean(n_sfx,  na.rm=TRUE),
              n_category = mean(n_category,  na.rm=TRUE),
              pos = paste(pos, collapse=","))
  saveRDS(morph_, file_u)
  print(morph_)
  }
}


compute_n_type <- function(metric_data){
  print("Computing number of types")
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_)
  tmp <- metric_data |>
    left_join(morph)  |>
    select(token, stem_m, pos)
  a <- tmp |>
    distinct() |>
    group_by(stem_m, pos) |>
    summarise(token_types = list(unique(token))) |>
    mutate(n_type = lengths(token_types)) |>
    filter(!is.na(stem_m)) |>
    filter(!stem_m =="")
  tmp |>
    left_join(a) |>
    mutate(token_types = ifelse(token_types == "NULL", NA, token_types)) |>
    group_by(token, pos) |>
    summarise(n_type = mean(n_type, na.rm=TRUE),
              token_types = list(unique(token_types)),
              token_stem = list(unique(na.omit(stem_m)))) |>
    filter(!is.na(n_type)) |>
    group_by(token) |>
    summarise(n_type = mean(n_type,  na.rm=TRUE),
              token_types = paste(token_types, collapse=","),
              token_stem = paste(token_stem, collapse=","))

}

compute_prefix <- function(metric_data){
  print("Computing prefixes")
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_)
  metric_data |>
    left_join(morph) |>
    mutate(prefix = ifelse(prefix_m == "", 0, 1)) |>
    group_by(token, pos) |>
    summarise(prefix = mean(prefix, na.rm=TRUE)) |>
    filter(!is.na(prefix)) |>
    group_by(token)|>
    summarise(prefix = mean(prefix, na.rm=TRUE))
}



default_metric_funs <- list(compute_count, compute_mlu, compute_positions, compute_length_char,
                            compute_length_phon, compute_n_type, compute_n_sfx_cat, compute_verb_frame, compute_prefix)

default_corpus_args <- list(corpus = NULL, role = NULL,
                            role_exclude = "Target_Child", age = NULL,
                            sex = NULL, part_of_speech = NULL, token = "*")

get_childes_data <- function(childes_lang, corpus_args) {

  file_t <- file.path(childes_path, glue("tokens_{childes_lang}.rds"))
  file_u <- file.path(childes_path, glue("utterances_{childes_lang}.rds"))

  if (file.exists(file_u)) {
    utterances <- readRDS(file_u)
  } else {
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
                              write = TRUE, import_data = NULL) {

  childes_lang <- convert_lang_childes(lang)
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

  metric_data <- tokens |> left_join(utterances)
  token_metrics <- map(metric_funs, \(fun) fun(metric_data)) |>
    reduce(partial(full_join, by = c("token"))) |>
    mutate(language = lang) |>
    mutate(freq_raw = count / sum(count))

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
      rename(uni_lemma = X1, definition = X2) |>
      pivot_longer(-c(uni_lemma, definition),
                   names_to = "x", values_to = "option") |>
      filter(!is.na(option)) |>
      select(-x) |>
      mutate(language = lang)
  } else {
    special_case_map <- tibble(language = character(), uni_lemma = character(),
                               definition = character(), option = character())
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
    group_by(language, uni_lemma, definition) |>
    summarise(special_cases = list(option))

  uni_lemmas |>
    unnest(items) |>
    left_join(special_case_map) |>
    mutate(option = pmap(list(language, definition, special_cases),
                         build_options)) |>
    select(language, uni_lemma, option) |>
    unnest(option) |>
    mutate(option = tolower(option))
}

get_uni_lemma_metrics <- function(lang, uni_lemma_map, import_data = NULL) {
  norm_lang <- normalize_language(lang)
  if (!is.null(import_data)) {
    token_metrics <- import_data
  } else {
    token_metrics_file <- glue("{childes_path}/token_metrics_{norm_lang}.rds")
    token_metrics <- readRDS(token_metrics_file)
  }

  tokens_mapped <- token_metrics |>
    select(token, token_stem) |>
    mutate(token_self = token,
           token_stemmed = stem(token, lang)) |>
    pivot_longer(c(token_self, token_stem, token_stemmed), names_to = "src",
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
    select(uni_lemma, tokens = token, where(is_numeric)) |>
    group_by(uni_lemma) |>
    distinct()

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
                       \(x) weighted.mean(x, freq_raw, na.rm = TRUE)))
  )

  uni_metrics <- metrics_summaries |>
    reduce(partial(left_join, by = "uni_lemma")) |>
    inner_join(uni_lemma_tokens) |>
    mutate(n_types = map_int(tokens, length), language = lang)

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
        rename(n_types = n_tokens)
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
