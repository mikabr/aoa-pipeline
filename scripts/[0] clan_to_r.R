read_cha <- function(file_path, transcript_id, corpus_info, tgt_child_replace = F) {
  # NOTE: this function ignores any additional tiers (e.g., %mor)
  file_contents <- tryCatch(
    readLines(file_path),
    error = \(e) {
      message(glue("Error reading file {file_path}: {e$message}"))
      return(data.frame())
    })
  if (tgt_child_replace & !any(str_detect(file_contents, "Target_Child"))) {
    file_contents <- file_contents |> str_replace_all("Child", "Target_Child")
  }

  # Participant data
  ppts <- file_contents |>
    str_subset("@ID:\t") |>
    str_replace("@ID:\t", "") |>
    I() |>
    read_delim(delim = "|",
               col_names = c("language", "corpus_name",
                             "speaker_code", "speaker_age", "speaker_sex",
                             "speaker_group", "speaker_eth_ses",
                             "speaker_role", "speaker_edu", "custom", "blank"),
               show_col_types = FALSE)
  target_child_name <- file_contents |>
    str_subset("@Participants:\t") |>
    str_extract("(?<=CHI ).*(?= Target_Child)")
  ta <- ppts |>
    filter(speaker_role == "Target_Child") |>
    pull(speaker_age) |>
    str_split(";") |>
    unlist() |>
    as.numeric()
  target_child_age <- case_when(
    length(ta) == 1 ~ ta,
    length(ta) == 2 ~ ta[1] * 12 + ta[2],
    length(ta) == 3 ~ ta[1] * 12 + ta[2] + ta[3] / (365.2425 / 12),
    .default = ta
  ) |> unique()
  target_child_sex <- ppts |>
    filter(speaker_role == "Target_Child") |>
    pull(speaker_sex)
  target_child_id <- file_path |>
    str_remove(glue(".*/{corpus_info$corpus_name}/")) |>
    str_remove("/.*") |>
    as.integer() |>
    {\(x) x + corpus_info$corpus_id * 100}()
  # transcript_id <- file_path |> basename() |> str_remove("\\.cha") |> as.integer()
  ppts <- ppts |>
    mutate(speaker_id = transcript_id * 10 + seq_along(speaker_code))

  utts <- file_contents |>
    str_subset("^\\*") |>
    str_replace_all("\\*", "") |>
    str_replace_all(":\t", "|") |>
    I() |>
    read_delim(delim = "|",
               col_names = c("speaker_code", "gloss"),
               show_col_types = FALSE)
  utterances_cleaned <- utts |>
    mutate(gloss = gloss |>
             str_replace_all("(?<=\\s|^)\\(.*\\)(?=\\s|$)", "") |>
             str_replace_all("@.*\\b", "") |>
             str_replace_all("\\[.*\\]", "") |>
             str_replace_all("[0-9_]+", ""),
           last_char = gloss |> str_split(" ") |> sapply(tail, 1),
           type = case_when(
             last_char == "." ~ "declarative",
             last_char == "?" ~ "question",
             last_char == "!" ~ "imperative_emphatic",
             last_char == "?!" ~ "question exclamation",
             last_char == "+..." ~ "trail off",
             last_char == "+/." ~ "interruption",
             last_char == "+/?" ~ "interruption question",
             last_char == "+//." ~ "self interruption",
             last_char == "+\"/." ~ "quotation next line",
             .default = "other"
           ),
           gloss = gloss |>
             str_replace_all("(?<=\\s|^)[:punct:]*(?=\\s|$)", "") |>
             str_squish()
    ) |>
    select(-last_char)

  if (nrow(utterances_cleaned) == 0) {
    return(data.frame())
  }

  utterances <- utterances_cleaned |>
    left_join(ppts |> select(speaker_code, language, speaker_role, speaker_id), by = "speaker_code") |>
    mutate(num_tokens = gloss |> str_split(" ") |> lengths(),
           utterance_order = 1:length(gloss),
           corpus_name = corpus_info$corpus_name,
           target_child_name = target_child_name,
           target_child_age = target_child_age,
           target_child_sex = target_child_sex,
           collection_name = "Other",
           collection_id = corpus_info$collection_id,
           corpus_id = corpus_info$corpus_id,
           target_child_id = target_child_id,
           transcript_id = transcript_id,
           stem = "",
           actual_phonology = "",
           model_phonology = "",
           num_morphemes = NA,
           part_of_speech = "",
           speaker_name = NA,
           media_start = NA,
           media_end = NA,
           media_unit = NA,
           id = transcript_id * 10000 + seq_along(gloss))|>
    select(id, gloss,
           stem, actual_phonology, model_phonology,
           type, language,
           num_morphemes,
           num_tokens, utterance_order, corpus_name,
           part_of_speech,
           speaker_code, speaker_name, speaker_role,
           target_child_name, target_child_age, target_child_sex,
           media_start, media_end, media_unit,
           collection_name, collection_id, corpus_id,
           speaker_id, target_child_id, transcript_id)

  utterances
}

make_tokens <- function(utterances) {
  tokens <- utterances |>
    mutate(gloss = lapply(gloss, \(g) {
      tibble(gloss = str_split(g, " ") |> unlist(),
             token_order = seq_along(gloss))
    })) |>
    unnest(gloss) |>
    mutate(utterance_id = .data$id,
           utterance_type = type,
           id = utterance_id * 100 + token_order,
           prefix = "",
           suffix = "",
           english = "",
           clitic = "") |>
    select(id,
           gloss, language, token_order,
           prefix, part_of_speech, stem,
           actual_phonology, model_phonology,
           suffix, num_morphemes, english, clitic,
           utterance_type, corpus_name,
           speaker_code, speaker_name, speaker_role,
           target_child_name, target_child_age, target_child_sex,
           collection_name, collection_id, corpus_id,
           speaker_id, target_child_id, transcript_id, utterance_id)
  tokens
}

