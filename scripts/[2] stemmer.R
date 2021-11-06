# Gets stems for a list of words in a given language.
# Uses Snowball by default (with special case for Croatian, which
# uses Steven Koch's implementation of the Zagreb Stemmer)
# Also allows for hunspell as an alternative method

stem <- function(words, language, method = "snowball") {
  # lang <- convert_lang_stemmer(language, method)

  if (method == "snowball") {

    if (language %in% SnowballC::getStemLanguages()) {
      SnowballC::wordStem(words, language)
    } else if (language == "Croatian") {
      chunk_size <- 1000
      word_chunks <- split(words, ceiling(seq_along(words) / chunk_size))
      cro_stemmer <- file.path(here::here(), "scripts/croatian.py")
      map(word_chunks, function(word_chunk) {
        system2("python",
                args = c(cro_stemmer, glue('"{word_chunk}"')),
                stdout = TRUE)
      }) |> unlist()
    }

  } else if (method == "hunspell") {

    Sys.setenv(DICPATH = "resources/dicts")

    if (language %in% hunspell::list_dictionaries()) {
      lapply(words, \(word) {
        stem <- hunspell::hunspell_stem(word, dictionary(language))[[1]]
        return(if (length(stem) == 0) word else stem[1])
      }) |> unlist()
    }

  } else {
    message(glue("invalid stemming method {method}"))
  }
}
