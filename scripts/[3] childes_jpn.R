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
