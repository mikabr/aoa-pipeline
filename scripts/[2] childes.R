default_corpus_args <- list(corpus = NULL, role = NULL,
                            role_exclude = "Target_Child", age = NULL,
                            sex = NULL, part_of_speech = NULL, token = "*")

get_childes_data <- function(childes_lang, corpus_args,
                             components = c("utterances", "tokens")) {
  if (childes_lang == "jpn") return(get_childes_data_jpn(corpus_args, components))
  # if (childes_lang == "ara") return(get_childes_data_ara(corpus_args))
  # if (childes_lang == "fin") return(get_childes_data_fin(corpus_args))

  components <- intersect(components, c("utterances", "tokens"))
  if (length(components) == 0) {
    stop("`components` must include 'utterances' and/or 'tokens'")
  }

  needs_process <- childes_lang %in% c("heb", "ara") &&
    !file.exists(here(childes_path, glue("tokens_{childes_lang}_orig.rds")))
  load_components <- if (needs_process) c("utterances", "tokens") else components

  file_t <- here(childes_path, glue("tokens_{childes_lang}.rds"))
  file_u <- here(childes_path, glue("utterances_{childes_lang}.rds"))

  utterances <- NULL
  tokens <- NULL

  if ("utterances" %in% load_components) {
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
      if (childes_lang == "yue") {
        utt2 <- get_utterances(language = "yue eng",
                               corpus = corpus_args$corpus,
                               role = corpus_args$role,
                               role_exclude = corpus_args$role_exclude,
                               age = corpus_args$age,
                               sex = corpus_args$sex)
        utterances <- bind_rows(utterances, utt2)
      }
      saveRDS(utterances, file_u)
    }
  }

  if ("tokens" %in% load_components) {
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
      if (childes_lang == "yue") {
        tok2 <- get_tokens(language = "yue eng",
                           corpus = corpus_args$corpus,
                           role = corpus_args$role,
                           role_exclude = corpus_args$role_exclude,
                           age = corpus_args$age,
                           sex = corpus_args$sex,
                           token = corpus_args$token)
        tokens <- bind_rows(tokens, tok2)
      }
      saveRDS(tokens, file_t)
    }
  }

  childes_data <- list("utterances" = utterances, "tokens" = tokens)

  if (needs_process) {
    childes_data <- process_childes(childes_data, childes_lang)
  }

  drop <- setdiff(c("utterances", "tokens"), components)
  if (length(drop) > 0) {
    childes_data[drop] <- list(NULL)
  }
  childes_data
}

compute_count <- function(metric_data) {
  print("Computing count...")
  metric_data |> count(token, name = "count")
}

compute_transcript_diversity <- function(metric_data) {
  print("Computing transcript diversity...")
  metric_data |> group_by(token) |> summarise(transcript_diversity = n_distinct(transcript_id))
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

compute_burstiness <- function(metric_data) {
  neg_log_likelihood <- function(beta, tau_values) {
    if (beta <= 0) return(Inf)

    v <- 1/mean(tau_values)
    a <- (v * gamma((beta + 1) / beta))^beta

    n <- length(tau_values)
    ll <- n * log(a) + n * log(beta) + (beta - 1) * sum(log(tau_values)) -
      a * sum(tau_values^beta)

    return(-ll)
  }

  fit_beta <- function(tau_values) {
    if (length(tau_values) < 2) return(NA_real_)
    optimize(f = neg_log_likelihood,
             interval = c(0.01, 10),
             tau_values = tau_values)$minimum
  }

  print("Computing burstiness...")

  metric_data |>
    ungroup() |>
    select(token, transcript_id) |>
    group_by(transcript_id) |>
    mutate(pos = row_number()) |>
    group_by(transcript_id, token) |>
    filter(n() > 2) |>
    arrange(pos, .by_group = TRUE) |>
    reframe(tau = diff(pos)) |>
    group_by(token) |>
    filter(n() >= 2) |>
    summarise(burstiness = fit_beta(tau), .groups = "drop")
}

compute_semantic_consistency <- function(metric_data) {
  print("Retrieving semantic consistency...")
  childes_lang <- metric_data$language[1]
  read_csv(here("data", "childes", glue("token_centroid_distances_{childes_lang}.csv")),
           show_col_types = FALSE) |>
    rename(semantic_consistency = cos_sim)
}
