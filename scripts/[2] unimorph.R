library(tidyverse)

extract_unimorph_data <- function(unimorph_lang) {
  base_file <- glue("resources/morphology/{unimorph_lang}.tsv")
  if(!file.exists(base_file)) {
    message(glue("No unimorph data for {unimorph_lang}, skipping"))
    return(NA)
  }
  morph_data <- read_tsv(base_file,
                         col_names = c("stem", "gloss", "morph_info")) |>
    mutate(morph_info = morph_info |>
             str_replace("^(N|V|ADJ)[;|]", "\\1-") |>
             # str_replace("^ADP\\+DET", "ADP+DET-") |> # not sure what the fxn of this is
             str_replace_all("[ |]", ";") |>
             str_replace(";$", "")) |>
    separate(morph_info, c("pos", "morph_info"), sep = "-", fill = "right") |>
    mutate(n_cat = morph_info |> str_split(";") |> lengths()) # num_morph_categories

  seg_file <- glue("resources/morphology/{unimorph_lang}.segmentations.tsv")
  if(!file.exists(seg_file)) {
    message(glue("No segmentation data for {unimorph_lang}, skipping"))
  } else {
    seg_data <- read_tsv(seg_file,
                         col_names = c("stem", "gloss", "morph_info",
                                       "segment_info")) |>
      mutate(n_morpheme = ifelse(segment_info == "", NA,
                                 segment_info |> str_split("\\|") |> lengths())) |>
      select(-morph_info)
    morph_data <- morph_data |>
      left_join(seg_data, by = c("stem", "gloss"))
  }

  der_file <- glue("resources/morphology/{unimorph_lang}.derivations.tsv")
  if(!file.exists(der_file)) {
    message(glue("No derivation data for {unimorph_lang}, skipping"))
  } else {
    der_data <- read_tsv(der_file,
                         col_names = c("stem", "gloss", "pos_der", "affix")) |>
      mutate(has_prefix = str_ends(affix, "-"))
    morph_data <- morph_data |>
      left_join(der_data, by = c("stem", "gloss")) |>
      mutate(is_derivation = !is.na(affix))
  }

  return(morph_data)
}

get_morph_data <- function(lang, corpus_args = default_corpus_args,
                           import_data = NULL) {

  childes_lang <- convert_lang_childes(lang)
  file_m <- file.path(childes_path, glue("morph_metrics_{childes_lang}.rds"))

  if (!is.null(import_data)) {
    childes_data <- import_data
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args)
  }

  unimorph_lang <- convert_lang_unimorph(lang)
  morph_data <- extract_unimorph_data(unimorph_lang)

  morph_data <- childes_data$tokens |>
    left_join(morph_data, by = "gloss") |>
    mutate(stem = coalesce(na_if(stem.x, ""), stem.y)) |>
    group_by(id, utterance_id, corpus_name, gloss) |>
    summarise(stem_m = stem |> unique() |> sort() |> paste(collapse = ", "),
              morph_info = morph_info |> unique() |> sort() |> paste(collapse = ", "),
              segment_info = if("segment_info" %in% colnames(morph_data)) {
                segment_info |> unique() |> sort() |> paste(collapse = ", ")} else NA,
              pos = pos |> unique() |> sort() |> paste(collapse = ", "),
              n_morpheme = if("n_morpheme" %in% colnames(morph_data)) {
                mean(n_morpheme)} else NA,
              n_cat  = mean(n_cat),
              is_derivation = if("is_derivation" %in% colnames(morph_data)) {
                max(is_derivation)} else NA,
              prefix_m = if("has_prefix" %in% colnames(morph_data)) {
                max(has_prefix)} else NA) |>
    distinct()

  saveRDS(morph_data, file_m)
  return(morph_data)
}

load_morph_data <- function(lang, corpus_args = default_corpus_args) {

  childes_lang <- convert_lang_childes(lang)
  file_m <- file.path(childes_path, glue("morph_metrics_{childes_lang}.rds"))

  if(file.exists(file_m)) {
    message(glue("Loading cached morphology data for {lang}."))
    morph_data <- readRDS(file_m)
  } else {
    message(glue("No cached morphology data for {lang}, getting and caching data."))
    morph_data <- get_morph_data(lang, corpus_args)
  }
  return(morph_data)
}
