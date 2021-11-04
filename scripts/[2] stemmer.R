# Gets stems for a list of words in a given language.
# Uses Snowball by default (with special case for Croatian, which
# uses Steven Koch's implementation of the Zagreb Stemmer)
# Also allows for hunspell as an alternative method

stem <- function(words, language, method = "snowball") {

  if (method == "snowball") {

    if (language %in% SnowballC::getStemLanguages()) {
      SnowballC::wordStem(words, language)

    } else if (language == "croatian") {
      chunk_size <- 1000
      word_chunks <- split(words, ceiling(seq_along(words) / chunk_size))
      map(word_chunks, function(word_chunk) {
        system2("python",
                args = c("scripts/croatian.py", sprintf('"%s"', word_chunk)),
                stdout = TRUE)
      }) |> unlist()

    } else {
      warning(sprintf("language %s not in list of stemmable languages",
                      language))
      words
    }

  } else if (method == "hunspell") {

    Sys.setenv(DICPATH = here("resources", "dicts"))

    if (language %in% hunspell::list_dictionaries()) {
      lapply(words, \(word) {
        stem <- hunspell::hunspell_stem(word, dictionary(language))[[1]]
        return(if (length(stem) == 0) word else stem[1])
      }) |> unlist()
    } else {
      warning(sprintf("language %s not in list of stemmable languages",
                      language))
      words
    }

  } else {
    warning(sprintf("invalid stemming method %s", method))
    words
  }
}
