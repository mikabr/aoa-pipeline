
# Gets stems for a list of words in a given language.
# Uses Snowball by default, no stemming for Chinese languages, and hunspell otherwise

stem <- function(words, language) {
  if (language %in% c("Mandarin (Beijing)", "Mandarin (Taiwanese)", "Cantonese")) {
    # no stemming
    return(words)
  } else if (language %in% c("Hebrew", "Korean", "Croatian", "Czech")) {
    # hunspell
    lang <- convert_lang_stemmer(language, "hunspell")
    if (lang %in% hunspell::list_dictionaries()) {
      map_chr(words, \(word) {
        stem <- hunspell::hunspell_stem(word, hunspell::dictionary(lang))[[1]]
        return(if (length(stem) == 0) word else stem[1])
      })
    } else {
      message(glue("Hunspell dictionary for {language} not installed"))
    }
  } else {
    # snowball
    lang <- convert_lang_stemmer(language, "snowball")
    if (lang %in% SnowballC::getStemLanguages()) {
      SnowballC::wordStem(words, lang)
    } else {
      message(glue("Language {language} has no Snowball stemmer"))
    }
  }
}
