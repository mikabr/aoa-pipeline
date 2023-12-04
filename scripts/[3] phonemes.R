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

count_phon_neighbors <- function(ipa, ipa_list, radius) {
  (adist(ipa, ipa_list |> unlist()) <= 2) |> sum() - 1
}

#some predictors are sensitive to the word, not the uni-lemma. Eg pronounciation
#For these cases, we get the predictor by word and then average by uni-lemma (eg a vs an)

# https://github.com/mikabr/aoa-prediction/blob/67764a7a4dfdd743278b8a56d042d25723dbdec7/aoa_unified/aoa_loading/aoa_loading.Rmd#L339

# clean_words(c("dog", "dog / cat", "dog (animal)", "(a) dog", "dog*", "dog(go)", "(a)dog", " dog ", "Cat"))
clean_words <- function(word_set){
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
    gsub("^ +| +$", "", .) %>%
    keep(nchar(.) > 0) |>
    tolower() |>
    unique()
}

map_phonemes <- function(uni_lemmas, method = "espeak-ng", radius = 2,
                         write = TRUE) {
  phon_path <- here("data", "predictors", "phonology.rds")
  if (file.exists(phon_path)) {
    message("Loading cached phonology data...")
    uni_phons_fixed <- readRDS(phon_path)
  } else {
    fixed_words <- read_csv("data/predictors/fixed_words.csv") |>
      select(language, uni_lemma, item_definition, fixed_word) |>
      filter(!is.na(uni_lemma), !is.na(fixed_word))

    uni_cleaned <- uni_lemmas |>
      unnest(cols = "items") |>
      # distinct(language, uni_lemma, definition) %>%
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

    if (write) {
      saveRDS(uni_phons_fixed, phon_path)
    }
  }
  uni_phons_fixed
}

compute_phon_metrics <- function(phon_data) {
  # compute phonological neighborhood
  phon_data <- phon_data |>
    nest(items = -language) |>
    mutate(items = lapply(items, \(w) {
      w |> mutate(phon_neighborhood = sapply(str_phons, \(x) {
        lapply(x, \(y) {
          count_phon_neighbors(y, w$str_phons, radius)
        }) |>
          unlist() |>
          mean(na.rm = T)
      }))
    })) |>
    unnest(cols = items)

  # get lengths
  uni_lengths <- phon_data |>
    mutate(num_char = num_chars(cleaned_words),
           num_phon = num_chars(str_phons)) |>
    group_by(language, uni_lemma) |>
    summarize(num_chars = mean(num_char, na.rm = TRUE),
              num_phons = mean(num_phon, na.rm = TRUE),
              phon_neighbors = mean(phon_neighborhood, na.rm = TRUE))
  return(uni_lengths)
}
