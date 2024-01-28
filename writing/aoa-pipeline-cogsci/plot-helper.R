target_langs <- c("Danish",
                  "Dutch",
                  "English (American)", "English (Australian)",
                  "English (British)",
                  "German",
                  "Norwegian",
                  "Swedish",
                  "Catalan",
                  "French (French)", "French (Quebecois)",
                  "Italian",
                  "Portuguese (European)",
                  "Spanish (Mexican)", "Spanish (European)",
                  "Croatian", "Czech", "Russian",
                  "Cantonese",
                  "Mandarin (Beijing)", "Mandarin (Taiwanese)",
                  "Estonian", "Hungarian",
                  "Japanese",
                  "Korean",
                  "Hebrew",
                  "Turkish")

display_predictors <- function(predictors) {
  predictors |>
    str_replace_all("b_", "") |>
    str_replace_all("_", " ") |>
    str_to_sentence() |>
    str_replace("Mlu", "MLU-w") |>
    str_replace("Length phon", "Length in phonemes") |>
    str_replace("Freq", "Frequency") |>
    str_replace("Cd", "Contextual diversity")
}

term_fct <- c("Frequency",
              "Concreteness", "Babiness",
              "Length in phonemes", "Phon neighbours",
              "N features", "Form entropy", "N morphemes",
              "Subcat entropy", "MLU-w")

ms_terms <- c("N features", "Form entropy", "N morphemes", "MLU-w")

lexcats <- c("Nouns", "Predicates", "Function words")

label_caps <- function(value) {
  value |>
    str_to_sentence() |>
    str_replace_all("_", " ")
}

theme_mikabr <- function(base_size = 14, base_family = "Open Sans") {
  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size,
                      #base_family = base_family
                      ),
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank())
  )
}

COR_SCALE <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

GERMANIC <- "#cc0000"
ROMANCE <- "#e69138"
SLAVIC <- "#f1c232"
SINOTIBETAN <- "#8db63d"
URALIC <- "#45816e"
JAPONIC <- "#3d85c6"
KOREANIC <- "#3d50c6"
SEMITIC <- "#674ea7"
TURKIC <- "#cd3d9a"

lang_cols <- c(rep(GERMANIC, 8),
               rep(ROMANCE, 7),
               rep(SLAVIC, 3),
               rep(SINOTIBETAN, 3),
               rep(URALIC, 2),
               JAPONIC, KOREANIC, SEMITIC, TURKIC)

FAM_SCALE <- c(GERMANIC, ROMANCE, SLAVIC, SINOTIBETAN,
               URALIC, JAPONIC, KOREANIC, SEMITIC, TURKIC)
