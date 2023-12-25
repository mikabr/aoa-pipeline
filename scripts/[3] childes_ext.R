library(tidyverse)
library(here)
library(glue)
library(childesr)
library(quanteda)

get_childes_data_jpn <- function(corpus_args) {
  # Get and unzip CHAT files from CHILDES
  jpn_path <- here("data", "childes", "Japanese")
  if (length(list.dirs(jpn_path)) == 0) {
    dir.create(jpn_path, showWarnings = FALSE)
    system(glue("/opt/homebrew/bin/wget -nd -N -r -np -A.zip -o 'wget.log' ",
                "-e robots=off -P {jpn_path} ",
                "https://childes.talkbank.org/data/Japanese/"))
    system(glue("/opt/homebrew/bin/wget -nd -N -r -np -A.zip -o 'wget.log' ",
                "-e robots=off -P {jpn_path} ",
                "https://phon.talkbank.org/data/Japanese/"))
    system("cd data/childes/Japanese/; unzip '*.zip'; rm *.zip")
  }


  # Get Japanese collection from childes-db
  utt_orig_fp <- here("data", "childes", "utterances_jpn_orig.rds")
  if (file.exists(utt_orig_fp)) {
    jpn_utterances <- readRDS(utt_orig_fp)
  } else {
    jpn_utterances <- get_utterances(collection = "Japanese",
                                     corpus = corpus_args$corpus,
                                     role = corpus_args$role,
                                     role_exclude = corpus_args$role_exclude,
                                     age = corpus_args$age,
                                     sex = corpus_args$sex)
    saveRDS(jpn_utterances, utt_orig_fp)
  }


  # Match orthographic representation to CHILDES
  utt_fp <- here("data", "childes", "utterances_jpn.rds")
  if (file.exists(utt_fp)) {
    utterances_df <- readRDS(utt_fp)
  } else {
    jpn_transcripts <- get_transcripts(collection = "Japanese")
    ortho_utterances <- jpn_transcripts |>
      mutate(ortho = lapply(filename, \(f) {
        transcript <- read_lines(here("data", "childes", str_replace(f, "\\.xml", "\\.cha")))
        ortho <- transcript[grep("%ort:\t", transcript)]
        ortho_df <- tibble(utterance_order = seq_along(ortho),
                           orthography = ortho) |>
          mutate(orthography = orthography |>
                   str_remove("%ort:\t") |>
                   str_replace_all("(&=?(warau)?| ?([[:punct:]]|\\[.\\]))| \\[=! [a-z]* \\]", " ") |>
                   str_squish())
      })) |>
      select(transcript_id, ortho) |>
      unnest(ortho)

    utterances_df <- jpn_utterances |>
      left_join(ortho_utterances, by = c("transcript_id", "utterance_order")) |>
      mutate(gloss = orthography) |>
      select(-orthography) |>
      filter(!is.na(gloss))

    saveRDS(utterances_df, utt_fp)
  }


  # Generate tokens dataframe
  # NOTE: We don't match with tokens from `get_tokens` because the gloss
  # representation sometimes doesn't line up with the orthographic representation;
  # this typically happens when one word (without spaces) in the orthography is
  # represented as two words (with a space in the middle) in the gloss, and there
  # is no straightforward way to identify these instances systematically.
  tok_fp <- here("data", "childes", "tokens_jpn.rds")
  if (file.exists(tok_fp)) {
    tokens_df <- readRDS(tok_fp)
  } else {

    make_tokens <- function(corpus, orthography) {
      o <- orthography
      if (is.na(o)) return(NA)
      if (corpus %in% c("Yokoyama", "NINJAL-Okubo", "Noji",
                        "Hamasaki", "Okayama", "MiiPro", "Miyata")) {
        return(tibble(orthography = str_split(o, " ") |> unlist(),
                      token_order = seq_along(orthography)))
      } else {
        return(tibble(orthography = quanteda::tokens(o)[[1]],
                      token_order = seq_along(orthography)))
      }
    }

    tokens_df <- utterances_df |>
      rename(utterance_type = type,
             utterance_id = id) |>
      mutate(tokens = map2(corpus_name, gloss, make_tokens)) |>
      unnest(tokens) |>
      mutate(prefix = "",
             suffix = "",
             english = "",
             clitic = "",
             id = seq_along(token_order)) |>
      select(id, gloss, language, token_order,
             prefix, part_of_speech, stem,
             actual_phonology, model_phonology,
             suffix, num_morphemes,
             english, clitic,
             utterance_type,
             corpus_name,
             speaker_code, speaker_name, speaker_role,
             target_child_name, target_child_age, target_child_sex,
             collection_name, collection_id, corpus_id,
             speaker_id, target_child_id,
             transcript_id, utterance_id, utterance_order, orthography) |>
      mutate(gloss = orthography) |>
      select(-orthography) |>
      filter(!is.na(gloss))

    if (!is.null(corpus_args$token) && !identical("*", corpus_args$token)) {
      token_string <- paste0("gloss %like% '", corpus_args$token, "'",
                             collapse = " | ")
      token_expr <- parse(text = token_string)[[1]]
      tokens_df <- tokens_df |> filter(!!token_expr)
    }
    saveRDS(tokens_df, tok_fp)
  }

  return(list("utterances" = utterances_df, "tokens" = tokens_df))
}

get_schema <- function(lang) {
  childes_lang <- convert_lang_childes(lang)
  read_csv(here("resources", glue("{childes_lang}_schemata.csv")))
}

process_childes_rus <- function(childes_data) {
  saveRDS(childes_data$tokens, here("data", "childes", "tokens_rus_orig.rds"))
  saveRDS(childes_data$utterances, here("data", "childes", "utterances_rus_orig.rds"))
  schema <- get_schema("Russian")
  exceptions <- read_csv(here("resources", "rus_exceptions.csv")) |>
    mutate(rus_orig = glue("\\b{rus_orig}\\b"))

  childes_data <- childes_data |>
    lapply(\(text) {
      text_new <- text |>
        nest(data = -corpus_name) |>
        mutate(data = map2(data, corpus_name, \(d, c) {
          d |> mutate(gloss = untransliterate(d$gloss, schema, c))
        })) |>
        unnest(data) |>
        mutate(gloss = str_replace_all(gloss,
                                       setNames(exceptions$rus_fix,
                                                exceptions$rus_orig)))

      text_new
    })
  childes_data$tokens <- childes_data$tokens |> select(id:utterance_type, everything())
  childes_data$utterances <- childes_data$utterances |> select(id:utterance_order, everything())
  saveRDS(childes_data$tokens, here("data", "childes", "tokens_rus.rds"))
  saveRDS(childes_data$utterances, here("data", "childes", "utterances_rus.rds"))
  childes_data
}

process_childes_heb <- function(childes_data) {
  saveRDS(childes_data$tokens, here("data", "childes", "tokens_heb_orig.rds"))
  saveRDS(childes_data$utterances, here("data", "childes", "utterances_heb_orig.rds"))
  schema <- get_schema("Hebrew")
  # for homophonous consonants, we construct all possible options and choose
  # the one with the highest corpus frequency

  tokens <- childes_data$tokens |>
    mutate(gloss = ifelse(!corpus_name %in% c("Ravid", "BermanLong"),
                          str_replace_all(gloss, c("(?<=\\b[aeiou])([aeiou])" = "ʔ\\1",
                                                   "([ae])(?=\\b)" = "\\1h",
                                                   "yi" = "y")),
                          gloss))

  heb_char_fix <- c(
    "\u200E" = "", # remove LTR mark
    "̄" = "", # remove long vowel mark
    "(?<=[\u05D0-\u05EA])כ$" = "ך",
    "(?<=[\u05D0-\u05EA])מ$" = "ם",
    "(?<=[\u05D0-\u05EA])נ$" = "ן",
    "(?<=[\u05D0-\u05EA])פ$" = "ף",
    "(?<=[\u05D0-\u05EA])צ$" = "ץ")

  exceptions <- read_csv(here("resources", "heb_exceptions.csv"))

  tokens_untrans <- tokens |>
    mutate(gloss = str_replace_all(gloss, setNames(exceptions$heb_fix,
                                                   exceptions$gloss))) |>
    nest(data = -corpus_name) |>
    mutate(data = map2(data, corpus_name, \(d, c) {
      d |> mutate(gloss = untransliterate(d$gloss, schema, c))
    })) |>
    unnest(data) |>
    mutate(gloss = str_replace_all(gloss, heb_char_fix))

  make_opts <- function(num_opts, gloss) {
    g <- gloss
    for (i in seq_len(num_opts)) {
      g <- sapply(g, str_replace, "\\[(.?)/(.?)\\]", c("\\1", "\\2")) |>
        as.vector()
    }
    g
  }

  tokens_opts <- tokens_untrans |>
    mutate(num_opts = str_count(gloss, "\\["),
           gloss = map2(num_opts, gloss, make_opts))

  # Hebrew frequencies from OpenSubtitles2018
  # via https://github.com/hermitdave/FrequencyWords/tree/master
  heb_freq <- read_delim(here("resources", "he_full.txt"),
                         delim = " ",
                         col_names = c("gloss", "freq"))

  tokens_final <- tokens_opts |>
    filter(num_opts > 0) |>
    unnest(gloss) |>
    left_join(heb_freq, by = "gloss") |>
    group_by(id) |>
    arrange(id, desc(freq)) |>
    slice(1) |>
    select(-freq)

  tokens_new <- tokens_opts |>
    left_join(tokens_final |> select(id, "final" = "gloss"), by = "id") |>
    mutate(gloss = ifelse(num_opts == 0, gloss, final) |> unlist()) |>
    select(-final, -num_opts)

  utterances_new <- childes_data$utterances |>
    left_join(tokens_new |>
                group_by(utterance_id) |>
                summarise(utterance = paste(gloss, collapse = " ")),
              by = c("id" = "utterance_id")) |>
    mutate(gloss = utterance) |>
    select(-utterance)

  childes_data$tokens <- tokens_new |> select(id:utterance_type, everything())
  childes_data$utterances <- utterances_new |> select(id:utterance_order, everything())
  saveRDS(childes_data$tokens, here("data", "childes", "tokens_heb.rds"))
  saveRDS(childes_data$utterances, here("data", "childes", "utterances_heb.rds"))
  childes_data
}
