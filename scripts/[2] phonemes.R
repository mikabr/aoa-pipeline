get_ipa <- function(word, lang, method = "espeak-ng") {
  lang_code <- convert_lang_espeak(lang, method)
  ipa <- system2("espeak", args = c("--ipa=3", "-v", lang_code, "-q", paste0('"', word, '"')),
                 stdout = TRUE) %>%
    gsub("^ ", "", .) %>%
    gsub("[ˈˌ0-9]", "", .)
  if(attr(ipa, "errmsg") |> length() == 0) { # lang exists for espeak(-ng)
    if(is_character(ipa, 1)) { # returns single string
      return(ipa)
    } else {
      message(glue("Error in processing '{word}' in {lang}"))
      return(NA_character_)
    }
  }
}

get_phons <- function(words, lang, method = "espeak-ng") {
  words |> map_chr(function(word) get_ipa(word, lang, method))
}

str_phons <- function(phon_words) {
  phon_words |> map(function(phon_word) {
    phon_word |>
      map_chr(~.x |>
                str_replace("r", "_r") |>
                str_replace("l", "_l") |>
                str_replace("ɹ", "_ɹ") |>
                str_replace("Q\"", "Q") |>
                str_replace("Q\\\"", "Q") |>
                str_split("[_ \\-]+") |>
                unlist() %>%
                keep(nchar(.) > 0 & !grepl("\\(.*\\)", .x)) |>
                paste(collapse = ""))
  })
}

num_chars <- function(words) {
  map_dbl(words, ~gsub("[[:punct:]]", "", .x) |> nchar() |> mean())
}

segment_ipa <- function(ipa) {
  if (length(ipa) != 1 || is.na(ipa) || !nzchar(ipa)) return(character())
  ipa <- str_remove_all(ipa, "[ˈˌ0-9]")
  chars <- stringi::stri_split_boundaries(ipa, type = "character")[[1]]
  chars <- chars[nzchar(chars)]
  if (length(chars) == 0) return(character())
  out <- chars[1]
  if (length(chars) == 1) return(out)
  for (ch in chars[-1]) {
    if (ch %in% c("ː", "ˑ", "͡") || str_ends(out[length(out)], "͡")) {
      out[length(out)] <- paste0(out[length(out)], ch)
    } else {
      out <- c(out, ch)
    }
  }
  out
}

n_segments <- function(ipa_strings) {
  ipa_strings <- unlist(ipa_strings)
  ipa_strings <- ipa_strings[!is.na(ipa_strings) & nzchar(ipa_strings)]
  if (length(ipa_strings) == 0) return(NA_real_)
  mean(map_dbl(ipa_strings, \(s) length(segment_ipa(s))))
}

phon_neighborhoods <- function(ipa_list, lemmas, radius = 2) {
  pronunciations <- map2(ipa_list, lemmas, \(ipa, lemma) {
    ipa <- unlist(ipa)
    ipa <- ipa[!is.na(ipa) & nzchar(ipa)]
    tibble(uni_lemma = lemma, ipa = ipa)
  }) |>
    list_rbind()

  if (nrow(pronunciations) == 0) return(rep(NA_real_, length(lemmas)))

  segs <- map(pronunciations$ipa, segment_ipa)
  inventory <- unique(unlist(segs))
  if (length(inventory) == 0) return(rep(NA_real_, length(lemmas)))
  codes <- intToUtf8(0xE000 + seq_along(inventory) - 1L, multiple = TRUE)
  names(codes) <- inventory
  pronunciations <- pronunciations |>
    mutate(encoded = map_chr(segs, \(p) {
      if (length(p) == 0 || anyNA(codes[p])) "" else paste0(codes[p], collapse = "")
    })) |>
    filter(nzchar(encoded))

  by_lemma <- pronunciations |>
    group_by(uni_lemma) |>
    summarise(forms = list(encoded), .groups = "drop")
  n <- nrow(by_lemma)
  counts <- map_dbl(seq_len(n), \(i) {
    self <- by_lemma$forms[[i]]
    others <- by_lemma$forms[-i]
    if (length(others) == 0) return(0)
    sum(map_dbl(others, \(o) min(adist(self, o))) <= radius)
  })
  names(counts) <- by_lemma$uni_lemma
  unname(counts[as.character(lemmas)])
}

compute_phon_metrics <- function(phon_data, radius = 2) {
  phon_data <- phon_data |>
    nest(items = -language) |>
    mutate(items = map(items, \(w) {
      w |> mutate(phon_neighborhood = phon_neighborhoods(str_phons, uni_lemma, radius))
    })) |>
    unnest(items)

  phon_data |>
    mutate(num_char = num_chars(cleaned_words),
           num_phon = map_dbl(str_phons, n_segments)) |>
    group_by(language, uni_lemma) |>
    summarise(num_chars = mean(num_char, na.rm = TRUE),
              num_phons = mean(num_phon, na.rm = TRUE),
              phon_neighbors = mean(phon_neighborhood, na.rm = TRUE),
              .groups = "drop")
}

# some predictors are sensitive to the word, not the uni-lemma, e.g. pronunciation
# for these cases, we get the predictor by word and then average by uni-lemma (e.g. a vs an)

# clean_words(c("dog", "dog / cat", "dog (animal)", "(a) dog", "dog*", "dog(go)", "(a)dog", " dog ", "Cat"))
clean_words <- function(word_set){
  word_set <- str_remove(word_set, "^[A-Z] Words for .+? -\\s*\\d+\\s*")
  word_set |>
    # dog / doggo -> c("dog", "doggo")
    strsplit("/") |> flatten_chr() |>
    # dog [dogs, doggo] -> c("dog", "dogs", "doggo")
    strsplit("[][,]") |> flatten_chr() |>
    # dog (animal) | (a) dog
    strsplit(" \\(.*\\)|\\(.*\\) ") |> flatten_chr() %>%
    strsplit("（.*）") |> flatten_chr() %>%
    # dog* | dog? | dog! | ¡dog! | dog's | dog…
    gsub("[*?!¡'…\\.，]", "", .) |>
    # dog(go) | (a)dog
    map_if(
      # if "dog(go)"
      ~grepl("\\(.*\\)", .x),
      # replace with "dog" and "doggo"
      ~c(sub("\\(.*\\)", "", .x),
         sub("(.*)\\((.*)\\)", "\\1\\2", .x))
    ) |>
    flatten_chr() %>%
    # trim
    gsub("^-+", "", .) %>%
    gsub("^ +| +$", "", .) %>%
    keep(nchar(.) > 0) |>
    tolower() |>
    unique()
}

checklist_boilerplate <- function(s) str_detect(s, "^[A-Z] Words for ")

map_phonemes <- function(uni_lemmas, method = "espeak-ng", radius = 2,
                         write = TRUE) {
  phon_path <- here("data", "predictors", "phonology.rds")
  if (file.exists(phon_path)) {
    message("Loading cached phonology data...")
    uni_phons_cached <- readRDS(phon_path)
    uni_cols <- colnames(uni_phons_cached)[1:5]
    uni_lemmas_new <- uni_lemmas |>
      unnest(cols = "items") |>
      left_join(uni_phons_cached,
                by = uni_cols) |>
      filter(sapply(phons, is.null) | checklist_boilerplate(item_definition)) |>
      select(all_of(uni_cols))

    if (nrow(uni_lemmas_new) == 0) return(uni_phons_cached)
  } else {
    uni_lemmas_new <- uni_lemmas |>
      unnest(cols = "items")
  }

  fixed_words <- read_csv("data/predictors/fixed_words.csv") |>
    select(language, uni_lemma, item_definition, fixed_word) |>
    filter(!is.na(uni_lemma), !is.na(fixed_word))

  uni_cleaned <- uni_lemmas_new |>
    left_join(fixed_words) |>
    mutate(fixed_definition = ifelse(is.na(fixed_word), item_definition, fixed_word),
           cleaned_words = map(fixed_definition, clean_words)) |>
    select(-fixed_word) |>
    group_by(language) |>
    #for each language, get the phonemes for each word
    mutate(phons = map2(cleaned_words, language, ~get_phons(.x, .y, method)))

  fixed_phons <- read_csv("data/predictors/fixed_phons.csv") |>
    select(language, uni_lemma, item_definition, fixed_phon) |>
    filter(!is.na(uni_lemma), !is.na(fixed_phon)) |>
    mutate(fixed_phon = strsplit(fixed_phon, ", "))

  uni_phons_fixed <- uni_cleaned |>
    left_join(fixed_phons) |>
    mutate(phons = if_else(map_lgl(fixed_phon, is.null), phons, fixed_phon),
           str_phons = str_phons(phons)) |>
    select(-fixed_phon)

  if (file.exists(phon_path)) {
    uni_phons_fixed <- uni_phons_fixed |>
      bind_rows(uni_phons_cached |> filter(!checklist_boilerplate(item_definition)))
  }

  if (write) {
    saveRDS(uni_phons_fixed, phon_path)
  }

  uni_phons_fixed
}
