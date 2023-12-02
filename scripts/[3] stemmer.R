# Gets stems for a list of words in a given language.
# Uses Snowball by default, no stemming for Chinese languages, and hunspell otherwise

stem <- function(words, lang) {
  if (lang %in% c("Mandarin (Beijing)", "Mandarin (Taiwanese)", "Cantonese")) {
    # no stemming
    return(words)
  } else if (lang %in% c("Hebrew", "Korean", "Croatian", "Czech")) {
    # hunspell
    hunspell_lang <- convert_lang_stemmer(lang, "hunspell")
    if (hunspell_lang %in% hunspell::list_dictionaries()) {
      map_chr(words, \(word) {
        if (grepl("ə", word)) return(word)
        stem <- hunspell::hunspell_stem(word, hunspell::dictionary(hunspell_lang))[[1]]
        return(if (length(stem) == 0) word else stem[1])
      })
    } else {
      message(glue("Hunspell dictionary for {lang} not installed"))
    }
  } else {
    # snowball
    snowball_lang <- convert_lang_stemmer(lang, "snowball")
    if (snowball_lang %in% SnowballC::getStemLanguages()) {
      SnowballC::wordStem(words, snowball_lang)
    } else {
      message(glue("Language {lang} has no Snowball stemmer"))
    }
  }
}

# Somehow lemmatizes all pronouns to il/son/se?
lemmatize <- function(words, lang) {
  udmodel <- get_udpipe_model(lang)
  udpipe(words, udmodel, parser = "none") |>
    mutate(doc_id = as.numeric(str_sub(doc_id, 4))) |>
    group_by(doc_id) |>
    summarise(lemma = paste(lemma, collapse = "+")) |>
    pull(lemma)
}
