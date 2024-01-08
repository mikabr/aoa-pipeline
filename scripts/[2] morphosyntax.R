library(udpipe)

get_udpipe_model <- function(language, overwrite = FALSE) {
  ud_lang <- convert_lang_udpipe(language)
  if (ud_lang == "cantonese-hk") {
    return(here("resources", "udpipe", "cantonese-hk-ud-2.12-231227.udpipe"))
  }
  dl <- udpipe_download_model(ud_lang,
                              model_dir = here("resources", "udpipe"),
                              overwrite = overwrite)
  dl$file_model
}

untransliterate <- function(text, schema, corpus) {
  schema_sorted <- schema |>
    rename(translit = !!corpus) |>
    arrange(desc(str_length(translit))) |>
    filter(complete.cases(translit)) |>
    mutate(original = replace_na(original, ""))
  str_replace_all(text |> tolower(),
                  setNames(schema_sorted$original,
                           schema_sorted$translit))
}

annotate_text <- function(text, language,
                          write = TRUE, num_cores = 4,
                          udmodel = NULL) {
  childes_lang <- convert_lang_childes(language)
  if (is.null(udmodel)) {
    udmodel <- get_udpipe_model(language) |>
      udpipe_load_model()
  }

  if (childes_lang == "swe") {
    # remove morpheme spacing symbols
    text <- gsub("[-+_]", "", text)
  }

  tokenizer = "tokenizer"
  if (childes_lang %in% c("zho", "yue eng")) {
    # use existing tokenization
    text <- gsub(" ", "\n", text)
    tokenizer = "vertical"
  }

  annotated <- text |>
    udpipe(udmodel, parallel.cores = num_cores,
           tokenizer = tokenizer)

  if (childes_lang == "kor") {
    # fix nonstandard parsing
    #
    # We consider the following to be verbs:
    # - pvg+ef
    # - pvg+ec*
    # - pvg+ep+ef
    # - pvg+ep+ec*
    # This explicitly excludes:
    # - Verb stems with denominal / deadjectival / adverbial endings
    # - Auxiliary verbs
    # - Copulas
    #
    # We also consider the lemma to be the first morpheme in the word
    # (first segment before any "+" chars)
    #
    # See:
    # https://arxiv.org/pdf/1309.1649.pdf (original convention)
    # https://aclanthology.org/W18-6013.pdf (proposed change)
    annotated <- annotated |>
      mutate(upos = ifelse(upos == "VERB", "VORIG", upos),
             upos = ifelse(grepl("^pvg\\+(ep\\+)?e[cf]", xpos), "VERB", upos),
             lemma = str_replace_all(lemma, "\\+.*", ""))
  }

  # fix pronouns
  annotated <- annotated |>
    mutate(lemma = ifelse(upos == "PRON", token, lemma))

  if (write) {
    file_p <- file.path(childes_path, glue("parsed_childes_{childes_lang}.rds"))
    saveRDS(annotated, file_p)
  }

  annotated
}

get_parsed_data <- function(lang, num_cores = 4,
                            corpus_args = default_corpus_args,
                            import_data = NULL) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- file.path(childes_path, glue("parsed_childes_{childes_lang}.rds"))

  if (!is.null(import_data)) {
    childes_data <- import_data
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args)
  }
  annotate_text(childes_data$utterances$gloss, lang, num_cores = num_cores)
}

load_parsed_data <- function(lang, corpus_args = default_corpus_args) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- file.path(childes_path, glue("parsed_childes_{childes_lang}.rds"))
  if(file.exists(file_p)) {
    message(glue("Loading cached parsed data for {lang}."))
    annotated <- readRDS(file_p)
  } else {
    message(glue("No cached parsed data for {lang}, getting and caching data."))
    annotated <- get_parsed_data(lang, corpus_args = default_corpus_args)
  }
  annotated
}

calculate_entropy <- function(counts) {
  counts |>
    table() |>
    as.data.frame() |>
    mutate(freq = Freq / sum(Freq),
           ent = freq * log2(freq)) |>
    pull(ent) |>
    sum() |>
    (`*`)(-1)
}

compute_form_entropy <- function(parsed_data) {
  print("Computing form entropy...")
  parsed_data |>
    select(token, lemma) |>
    nest(tokens = token) |>
    mutate(form_entropy = sapply(tokens, calculate_entropy)) |>
    select(-lemma) |>
    unnest(tokens) |>
    group_by(token) |>
    summarise(form_entropy = mean(form_entropy))
}

compute_subcat_entropy <- function(parsed_data) {
  print("Computing subcategorization frame entropy...")

  frames <- parsed_data |>
    select(doc_id, head_token_id, dep_rel) |>
    mutate(dep_rel = dep_rel |>
             str_replace_all(":.*", "")) |>
    nest(subcat = dep_rel) |>
    mutate(subcat = sapply(subcat, \(d) {
      unlist(d, use.names= FALSE) |>
        intersect(c("obj", "iobj", "ccomp", "xcomp", "obl")) |>
        paste(collapse = "_")}))
  items <- parsed_data |>
    # filter(upos == "VERB") |>
    left_join(frames, by = c("doc_id", "token_id" = "head_token_id")) |>
    # removes character(0) and NULL, both of which indicate no dependents under consideration
    mutate(subcat = ifelse(lengths(subcat) == 0, NA, subcat)) |>
    select(lemma, subcat) |>
    nest(subcats = subcat) |>
    mutate(subcat_entropy = sapply(subcats, calculate_entropy)) |>
    select(-subcats)
  parsed_data |>
    select(token, lemma) |>
    distinct() |>
    left_join(items, by = "lemma") |>
    select(-lemma) |>
    group_by(token) |>
    summarise(subcat_entropy = mean(subcat_entropy))
}

compute_n_features <- function(parsed_data) {
  print("Computing number of morphosyntactic features")

  parsed_data |>
    mutate(n_features = feats |> str_split("\\|") |> lengths()) |>
    group_by(token) |>
    summarise(n_features = mean(n_features, na.rm = TRUE)) |>
    distinct()
}

compute_n_morphemes <- function(morph_data) {
  print("Computing number of morphemes")

  morph_data |>
    rename(token = gloss) |>
    group_by(token) |>
    summarise(n_morphemes = mean(n_morpheme, na.rm = TRUE),
              # n_features = mean(n_cat, na.rm = TRUE),
              # pos = pos
              ) |>
    distinct()
}
