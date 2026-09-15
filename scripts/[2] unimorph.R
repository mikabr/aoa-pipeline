# NOTES:
# - zho morphology data is artificially constructed from CHILDES tokens,
#   mainly to reduce the complexity of the processing pipeline.
# - jpn morphology data is adapted from the SIGMORPHON 2023 shared task

# UniMorph lists multiple analyses per (stem, gloss) — e.g. 3rded as V;PST and
# V;V.PTCP;PST, and the same split in segmentations. Joining those tables on
# (stem, gloss) is many-to-many and cartesian-explodes (deu: ~115k keys duplicated
# on both sides, up to 30×30). Downstream metrics are per CHILDES gloss, so
# collapse each source to one row per gloss before joining.
extract_unimorph_data <- function(unimorph_lang, glosses = NULL) {
  base_file <- here("resources", "morphology", glue("{unimorph_lang}.tsv"))
  if(!file.exists(base_file)) {
    message(glue("No unimorph data for {unimorph_lang}, skipping"))
    return(NA)
  }

  keep_glosses <- function(df) {
    df <- mutate(df, gloss = tolower(gloss))
    if (is.null(glosses)) df else filter(df, gloss %in% glosses)
  }

  morph_data <- read_tsv(base_file,
                         col_names = c("stem", "gloss", "morph_info"),
                         col_types = "ccc",
                         show_col_types = FALSE) |>
    keep_glosses() |>
    mutate(morph_info = morph_info |>
             str_replace("^(N|V|ADJ)[;|]", "\\1-") |>
             str_replace_all("[ |]", ";") |>
             str_replace(";$", "")) |>
    separate(morph_info, c("pos", "morph_info"), sep = "-", fill = "right") |>
    mutate(n_cat = str_count(replace_na(morph_info, ""), ";") + 1) |>
    group_by(gloss) |>
    summarise(
      stem = stem |> unique() |> sort() |> paste(collapse = ", "),
      morph_info = morph_info |> unique() |> sort() |> paste(collapse = ", "),
      pos = pos |> unique() |> sort() |> paste(collapse = ", "),
      n_cat = mean(n_cat, na.rm = TRUE),
      .groups = "drop"
    )

  seg_file <- here("resources", "morphology", glue("{unimorph_lang}.segmentations.tsv"))
  if(!file.exists(seg_file)) {
    message(glue("No segmentation data for {unimorph_lang}, skipping"))
  } else {
    seg_data <- read_tsv(seg_file,
                         col_names = c("stem", "gloss", "morph_info",
                                       "segment_info"),
                         col_types = "cccc",
                         show_col_types = FALSE) |>
      keep_glosses() |>
      mutate(
        gloss = gloss,
        n_morpheme = if_else(segment_info == "" | is.na(segment_info),
                             NA_real_,
                             str_count(segment_info, "\\|") + 1),
        .keep = "none"
      ) |>
      group_by(gloss) |>
      summarise(n_morpheme = mean(n_morpheme, na.rm = TRUE), .groups = "drop")
    morph_data <- morph_data |>
      full_join(seg_data, by = "gloss", relationship = "one-to-one")
  }

  der_file <- here("resources", "morphology", glue("{unimorph_lang}.derivations.tsv"))
  if(!file.exists(der_file)) {
    message(glue("No derivation data for {unimorph_lang}, skipping"))
  } else {
    der_data <- read_tsv(der_file,
                         col_names = c("stem", "gloss", "pos_der", "affix"),
                         col_types = "cccc",
                         show_col_types = FALSE) |>
      keep_glosses() |>
      mutate(gloss = gloss,
             has_prefix = str_ends(affix, "-"),
             .keep = "none") |>
      group_by(gloss) |>
      summarise(
        prefix_m = any(has_prefix, na.rm = TRUE),
        is_derivation = TRUE,
        .groups = "drop"
      )
    morph_data <- morph_data |>
      full_join(der_data, by = "gloss", relationship = "one-to-one") |>
      mutate(is_derivation = replace_na(is_derivation, FALSE))
  }

  if (!"n_morpheme" %in% names(morph_data)) {
    morph_data <- morph_data |> mutate(n_morpheme = NA_real_)
  }
  morph_data
}

childes_type_counts <- function(childes_lang, corpus_args = default_corpus_args,
                                import_data = NULL) {
  if (!is.null(import_data)) {
    tokens <- import_data$tokens
  } else {
    childes_data <- get_childes_data(childes_lang, corpus_args,
                                     components = "tokens")
    tokens <- childes_data$tokens
    rm(childes_data)
    gc()
  }
  counts <- tokens |>
    filter(gloss != "") |>
    mutate(gloss = tolower(gloss), .keep = "none") |>
    count(gloss, name = "count")
  rm(tokens)
  gc()
  counts
}

get_morph_data <- function(lang, corpus_args = default_corpus_args,
                           import_data = NULL) {

  childes_lang <- convert_lang_childes(lang)
  file_m <- here(childes_path, glue("morph_metrics_{childes_lang}.rds"))
  dir.create(childes_path, showWarnings = FALSE, recursive = TRUE)

  type_counts <- childes_type_counts(childes_lang, corpus_args, import_data)

  unimorph_lang <- convert_lang_unimorph(lang)
  morph_data <- extract_unimorph_data(unimorph_lang, glosses = type_counts$gloss)
  if (length(morph_data) == 1 && is.na(morph_data)) {
    morph_data <- tibble(gloss = character(), n_morpheme = numeric(),
                         count = integer())
    saveRDS(morph_data, file_m)
    return(morph_data)
  }

  # Type-level UniMorph joined to CHILDES type counts (not every token row).
  # Occurrence-weighting of n_morphemes is weighted.mean(., count) in
  # compute_n_morphemes; unilemma aggregation still weights by the same counts.
  morph_data <- type_counts |>
    left_join(morph_data, by = "gloss", relationship = "one-to-one")

  saveRDS(morph_data, file_m)
  return(morph_data)
}

load_morph_data <- function(lang, corpus_args = default_corpus_args) {

  childes_lang <- convert_lang_childes(lang)
  file_m <- here(childes_path, glue("morph_metrics_{childes_lang}.rds"))

  if(file.exists(file_m)) {
    # Token-level English caches are multi-GB; rebuild rather than loading them.
    if (!is.na(file.info(file_m)$size) && file.info(file_m)$size > 200 * 1024^2) {
      message(glue("Cached morphology file for {lang} is large (likely token-level); ",
                   "rebuilding a type-level cache."))
      return(get_morph_data(lang, corpus_args))
    }
    message(glue("Loading cached morphology data for {lang}."))
    morph_data <- readRDS(file_m)
    # Old caches were one row per CHILDES token; collapse if we can do so
    # without holding a second copy (skip if already type-level).
    if ("utterance_id" %in% names(morph_data)) {
      message("Cached morph data is token-level; collapsing to unique glosses.")
      morph_data <- morph_data |>
        group_by(gloss) |>
        summarise(n_morpheme = mean(n_morpheme, na.rm = TRUE),
                  count = n(),
                  .groups = "drop")
      saveRDS(morph_data, file_m)
    }
    if (!"count" %in% names(morph_data)) {
      message("Cached morph data has no CHILDES type counts; rebuilding.")
      return(get_morph_data(lang, corpus_args))
    }
  } else {
    message(glue("No cached morphology data for {lang}, getting and caching data."))
    morph_data <- get_morph_data(lang, corpus_args)
  }
  return(morph_data)
}
