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
                          num_cores = 1,
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

  annotated
}

# udpipe() on a character vector labels documents "doc1", "doc2", ...;
# a data.frame input may already use integer doc_id.
udpipe_doc_index <- function(doc_id) {
  as.integer(str_remove(as.character(doc_id), "^doc"))
}

parsed_chunk_dir <- function(childes_lang) {
  here(childes_path, glue("parsed_childes_{childes_lang}"))
}

list_parsed_chunk_files <- function(childes_lang) {
  dir <- parsed_chunk_dir(childes_lang)
  if (!dir.exists(dir)) return(character())
  list.files(dir, pattern = "^chunk_[0-9]+\\.rds$", full.names = TRUE) |>
    sort()
}

pack_transcript_chunks <- function(utterances, chunk_size = 10000) {
  counts <- utterances |> count(transcript_id, name = "n_utt")
  chunk_id <- integer(nrow(counts))
  current <- 1L
  filled <- 0L
  for (i in seq_len(nrow(counts))) {
    if (filled >= chunk_size && filled > 0) {
      current <- current + 1L
      filled <- 0L
    }
    chunk_id[i] <- current
    filled <- filled + counts$n_utt[i]
  }
  counts |> mutate(chunk_id = chunk_id)
}

slim_parsed_cols <- function(annotated) {
  annotated |>
    select(any_of(c("doc_id", "token_id", "token", "lemma", "xpos", "feats",
                    "head_token_id", "dep_rel", "utterance_id", "transcript_id")))
}

combine_parsed_chunks <- function(childes_lang) {
  chunk_files <- list_parsed_chunk_files(childes_lang)
  if (length(chunk_files) == 0) {
    stop(glue("No parsed chunk files for '{childes_lang}'"))
  }
  file_p <- here(childes_path, glue("parsed_childes_{childes_lang}.rds"))
  chunk_dir <- parsed_chunk_dir(childes_lang)
  message(glue("Combining {length(chunk_files)} chunks into {basename(file_p)}"))
  combined <- map_dfr(seq_along(chunk_files), \(i) {
    message(glue("  reading chunk {i}/{length(chunk_files)}"))
    readRDS(chunk_files[[i]])
  })
  saveRDS(combined, file_p)
  unlink(chunk_dir, recursive = TRUE)
  message(glue("Wrote {file_p} and removed {chunk_dir}"))
  combined
}

get_parsed_data <- function(lang, num_cores = 1,
                            corpus_args = default_corpus_args,
                            import_data = NULL,
                            chunk_size = 10000,
                            overwrite = FALSE) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- here(childes_path, glue("parsed_childes_{childes_lang}.rds"))
  if (!overwrite && file.exists(file_p) &&
      length(list_parsed_chunk_files(childes_lang)) == 0) {
    message(glue("Parsed data for {lang} already cached."))
    return(readRDS(file_p))
  }

  chunk_dir <- parsed_chunk_dir(childes_lang)
  dir.create(chunk_dir, showWarnings = FALSE, recursive = TRUE)
  plan_file <- file.path(chunk_dir, "chunk_plan.rds")
  if (!overwrite && file.exists(plan_file)) {
    packed_existing <- readRDS(plan_file)
    n_done <- length(list_parsed_chunk_files(childes_lang))
    if (n_done >= max(packed_existing$chunk_id)) {
      message(glue("All {n_done} chunks present for {lang}; combining."))
      return(combine_parsed_chunks(childes_lang))
    }
  }

  if (!is.null(import_data)) {
    utterances <- import_data$utterances
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args,
                                     components = "utterances")
    utterances <- childes_data$utterances
    rm(childes_data)
    gc()
  }

  utterances <- utterances |>
    select(id, gloss, transcript_id) |>
    filter(!is.na(gloss), gloss != "")

  if (nrow(utterances) == 0) {
    message(glue("No utterances to parse for {lang}."))
    return(invisible(tibble()))
  }

  existing_chunks <- list_parsed_chunk_files(childes_lang)
  if (!overwrite && file.exists(plan_file)) {
    packed <- readRDS(plan_file)
  } else if (!overwrite && length(existing_chunks) > 0) {
    # Resume a run started before chunk plans (default chunk_size was 800).
    packed <- pack_transcript_chunks(utterances, chunk_size = 800)
    saveRDS(packed, plan_file)
  } else {
    packed <- pack_transcript_chunks(utterances, chunk_size = chunk_size)
    saveRDS(packed, plan_file)
  }
  n_chunks <- max(packed$chunk_id)
  message(glue("Parsing {nrow(utterances)} utterances in {n_chunks} ",
               "transcript-sized chunks for {lang} ",
               "(CHILDES '{childes_lang}'). Existing chunks are skipped."))

  udmodel <- get_udpipe_model(lang) |> udpipe_load_model()

  for (i in seq_len(n_chunks)) {
    chunk_file <- file.path(chunk_dir, glue("chunk_{str_pad(i, width = 5, pad = '0')}.rds"))
    if (!overwrite && file.exists(chunk_file)) {
      message(glue("Skipping existing chunk {i}/{n_chunks}"))
      next
    }
    tr_ids <- packed$transcript_id[packed$chunk_id == i]
    utt_chunk <- utterances |> filter(transcript_id %in% tr_ids)
    message(glue("Parsing chunk {i}/{n_chunks} ",
                 "({nrow(utt_chunk)} utterances)"))
    annotated <- annotate_text(utt_chunk$gloss, lang,
                               num_cores = num_cores,
                               udmodel = udmodel)
    doc_idx <- udpipe_doc_index(annotated$doc_id)
    if (anyNA(doc_idx) || any(doc_idx < 1L | doc_idx > nrow(utt_chunk))) {
      stop(glue("Could not map udpipe doc_id to utterances in chunk {i} ",
                "(example doc_id: {annotated$doc_id[1]})."))
    }
    annotated <- annotated |>
      mutate(utterance_id = utt_chunk$id[doc_idx],
             transcript_id = utt_chunk$transcript_id[doc_idx]) |>
      slim_parsed_cols()
    saveRDS(annotated, chunk_file)
    rm(annotated, utt_chunk)
    gc()
  }

  combine_parsed_chunks(childes_lang)
}

load_parsed_data <- function(lang, corpus_args = default_corpus_args) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- here(childes_path, glue("parsed_childes_{childes_lang}.rds"))
  if (file.exists(file_p)) {
    message(glue("Loading cached parsed data for {lang}."))
    return(readRDS(file_p))
  }
  message(glue("No cached parsed data for {lang}, getting and caching data."))
  get_parsed_data(lang, corpus_args = corpus_args)
}

entropy_from_counts <- function(freqs) {
  if (length(freqs) == 0) return(0)
  probs <- as.numeric(freqs) / sum(freqs)
  probs <- probs[probs > 0]
  -sum(probs * log2(probs))
}

calculate_entropy <- function(obs) {
  if (length(obs) == 0) return(0)
  entropy_from_counts(table(obs))
}

compute_form_entropy <- function(parsed_data) {
  print("Computing form entropy...")
  lemma_ent <- parsed_data |>
    count(lemma, token) |>
    group_by(lemma) |>
    summarise(form_entropy = entropy_from_counts(n), .groups = "drop")
  parsed_data |>
    distinct(token, lemma) |>
    inner_join(lemma_ent, by = "lemma") |>
    group_by(token) |>
    summarise(form_entropy = mean(form_entropy), .groups = "drop")
}

compute_subcat_entropy <- function(parsed_data) {
  print("Computing subcategorization frame entropy...")

  frames <- parsed_data |>
    filter(str_detect(dep_rel, "^(obj|iobj|ccomp|xcomp|obl|acl|nmod)"),
           !str_detect(dep_rel, "nmod:poss")) |>
    mutate(dep_rel_clean = str_extract(dep_rel, "^[^:]+")) |>
    group_by(utterance_id, head_token_id) |>
    summarise(subcat = paste(dep_rel_clean, collapse = "_"), .groups = "drop")
  items <- parsed_data |>
    select(utterance_id, token_id, lemma) |>
    left_join(frames, by = c("utterance_id", "token_id" = "head_token_id")) |>
    mutate(subcat = replace_na(subcat, "none")) |>
    count(lemma, subcat) |>
    group_by(lemma) |>
    summarise(subcat_entropy = entropy_from_counts(n), .groups = "drop")
  parsed_data |>
    distinct(token, lemma) |>
    left_join(items, by = "lemma") |>
    group_by(token) |>
    summarise(subcat_entropy = mean(subcat_entropy), .groups = "drop")
}

compute_mdd <- function(parsed_data) {
  print("Computing mean dependency distance")

  dep_dist <- parsed_data |>
    mutate(dist = ifelse(head_token_id == "0", 0,
                         abs(as.numeric(head_token_id) - as.numeric(token_id))
                         ))
  dep_dist |>
    group_by(lemma) |>
    summarise(mdd = mean(dist, na.rm = T)) |>
    rename(token = lemma)
}

compute_n_features <- function(parsed_data) {
  print("Computing number of morphosyntactic features...")

  parsed_data |>
    mutate(token = token,
           n_features = str_count(replace_na(feats, ""), "\\|") + 1,
           .keep = "none") |>
    group_by(token) |>
    summarise(n_features = mean(n_features, na.rm = TRUE), .groups = "drop")
}

compute_n_morphemes <- function(morph_data) {
  print("Computing number of morphemes...")
  has_count <- "count" %in% names(morph_data)
  morph_data |>
    mutate(token = gloss) |>
    group_by(token) |>
    summarise(
      n_morphemes = if (has_count) {
        weighted.mean(n_morpheme, count, na.rm = TRUE)
      } else {
        mean(n_morpheme, na.rm = TRUE)
      },
      .groups = "drop"
    )
}

compute_contextual_diversity <- function(parsed_data) {
  print("Computing contextual diversity...")

  # Chang filtered to top 5000 words + all CDI words
  lemmas <- read_csv(here("resources", "lemmas.csv"),
                     show_col_types = FALSE) |>
    filter(language == parsed_data$language[1])

  freqs <- parsed_data |>
    ungroup() |>
    count(lemma, name = "count")

  lemmas_included <- union(
    lemmas$lemma,
    freqs |> arrange(desc(count)) |> slice_head(n = 5000) |> pull(lemma)
  )
  n_incl <- length(lemmas_included)

  # Window size 5 (current + 4 following). NA ids stay in the stream so
  # lags skip non-included tokens rather than collapsing the sequence.
  stream <- parsed_data |> ungroup()
  lemma_id <- match(stream$lemma, lemmas_included)
  add_lag <- function(ids, k) {
    n <- length(ids)
    if (n <= k) return(NULL)
    a <- ids[seq_len(n - k)]
    b <- ids[(k + 1L):n]
    ok <- !is.na(a) & !is.na(b)
    if (!any(ok)) return(NULL)
    list(i = a[ok], j = b[ok])
  }
  lags <- split(lemma_id, stream$transcript_id) |>
    map(\(ids) map(1:4, \(k) add_lag(ids, k))) |>
    list_flatten() |>
    compact()
  i <- unlist(map(lags, "i"), use.names = FALSE)
  j <- unlist(map(lags, "j"), use.names = FALSE)
  M <- Matrix::sparseMatrix(
    i = i, j = j, x = 1,
    dims = c(n_incl, n_incl)
  )
  lemma_counts <- tibble(
    lemma = lemmas_included,
    context_lemmas = as.numeric(Matrix::rowSums(M > 0))
  ) |>
    inner_join(freqs, by = "lemma") |>
    filter(context_lemmas > 0) |>
    mutate(log_freq = log((count + 1) / sum(count)))

  lemma_reg <- tryCatch(
    nls(context_lemmas ~ SSlogis(log_freq, upper, xmid, scale),
        data = lemma_counts,
        control = nls.control(maxiter = 1e5,
                              minFactor = 1/4096)),
    error = function(e) {
      message("Fitting SSlogis failed, trying nls with custom upper bound.")
      upper_bound <- max(lemma_counts$context_lemmas) * 2

      nls(context_lemmas ~ upper_bound / (1 + exp(-(log_freq - xmid)/scale)),
          data = lemma_counts,
          start = list(xmid = -6, scale = 1))
    }
  )

  lemma_counts <- lemma_counts |>
    mutate(context_diversity = residuals(lemma_reg))

  parsed_data |>
    select(token, lemma) |>
    distinct() |>
    left_join(lemma_counts, by = "lemma") |>
    select(-lemma) |>
    filter(!is.nan(context_diversity)) |>
    group_by(token) |>
    summarise(context_diversity = mean(context_diversity, na.rm = TRUE))
}

prepare_parsed_chunk <- function(parsed, lang) {
  parsed <- parsed |> mutate(language = lang)
  if (lang == "Korean" && "xpos" %in% names(parsed)) {
    parsed <- parsed |> mutate(feats = str_replace_all(xpos, "\\+", "|"))
  }
  parsed
}

compute_parsed_metrics_from_chunks <- function(lang, metric_funs,
                                               corpus_args = default_corpus_args) {
  parsed <- load_parsed_data(lang, corpus_args = corpus_args) |>
    prepare_parsed_chunk(lang)
  map(metric_funs, \(fun) {
    out <- fun(parsed)
    gc()
    out
  })
}
