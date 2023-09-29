library(udpipe)

get_udpipe_model <- function(language, overwrite = FALSE) {
  ud_lang <- convert_lang_udpipe(language)
  dl <- udpipe_download_model(ud_lang,
                              model_dir = here("resources", "udpipe"),
                              overwrite = overwrite)
  dl$file_model
}

annotate_text <- function(text, language, # by_utterance = TRUE,
                          write = TRUE, num_cores = 4) {
  childes_lang <- convert_lang_childes(language)
  udmodel <- get_udpipe_model(language) |>
    udpipe_load_model()

  if (childes_lang == "swe") {
    # remove morpheme spacing symbols
    text <- gsub("[-+_]", "", text)
  }

  tokenizer = "tokenizer"
  if (childes_lang == "zho") {
    # use existing tokenization
    text <- gsub(" ", "\n", text)
    tokenizer = "vertical"
  }

  # if (!by_utterance) {
  #   text <- text |>
  #     nest(data = -utterance_id) |>
  #     mutate(text = sapply(data, \(x) {
  #       paste(x$gloss, collapse = "\n")
  #     }))
  #   tokenizer = "vertical"
  # }

  annotated <- text |>
    udpipe(udmodel, parallel.cores = num_cores,
           tokenizer = tokenizer)

  if (childes_lang == "kor") {
    # fix nonstandard parsing
    annotated <- annotated |>
      mutate(upos = ifelse(upos == "VERB", "VORIG", upos),
             upos = ifelse(grepl("^pvg\\+(ep\\+)?e[cf]", xpos), "VERB", upos),
             lemma = str_replace_all(lemma, "\\+.*", ""))
  }

  if (write) {
    file_p <- file.path(childes_path, glue("parsed_childes_{childes_lang}.rds"))
    saveRDS(annotated, file_p)
  }

  annotated
}

get_parsed_data <- function(lang, num_cores = 4,
                            corpus_args = default_corpus_args) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- file.path(childes_path, glue("parsed_childes_{childes_lang}.rds"))

  if(file.exists(file_p)) {
    message(glue("Loading cached parsed data for {lang}."))
    annotated <- readRDS(file_p)
  } else {
    message(glue("No cached parsed data for {lang}, getting and caching data."))
    childes_data <- get_childes_data(childes_lang, corpus_args)
    annotated <- annotate_text(childes_data$utterances$gloss, lang,
                               num_cores = num_cores)
  }
  annotated
}

compute_form_entropy <- function(parsed_data) {
  print("Computing form entropy...")
  parsed_data |>
    select(token, lemma) |>
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
    select(-lemma) |>
    unnest(tokens) |>
    distinct()
}

compute_subcat_entropy <- function(parsed_data) {
  verbs <- parsed_data |> filter(upos == "VERB")
  frames <- parsed_data |>
    select(doc_id, head_token_id, dep_rel) |>
    mutate(dep_rel = dep_rel |>
             str_replace_all(":.*", "")) |>
    nest(subcat = dep_rel) |>
    mutate(subcat = sapply(subcat, \(d) {
      unlist(d, use.names= FALSE) |>
        intersect(c("obj", "iobj", "ccomp", "xcomp", "obl")) |>
        paste(collapse = "_")}))
  verbs <- verbs |>
    left_join(frames, by = c("doc_id", "token_id" = "head_token_id")) |>
    # removes character(0) and NULL, both of which indicate no dependents under consideration
    mutate(subcat = ifelse(lengths(subcat) == 0, NA, subcat)) |>
    select(lemma, subcat) |>
    nest(subcats = subcat) |>
    mutate(subcat_entropy = sapply(subcats, \(s) {
      s |>
        table() |>
        as.data.frame() |>
        mutate(freq = Freq / sum(Freq),
               ent = freq * log2(freq)) |>
        pull(ent) |>
        sum() |>
        (`*`)(-1)
    })) |>
    select(-subcats)
  parsed_data |>
    select(token, lemma) |>
    distinct() |>
    left_join(verbs, by = "lemma") |>
    select(-lemma)
}
