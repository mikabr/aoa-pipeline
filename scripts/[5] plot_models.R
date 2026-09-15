# Plot helpers

# library(showtext)
# font_add_google("Open Sans", "opensans")
# showtext_auto()

label_caps <- function(value) {
  value |>
    str_to_sentence() |>
    str_replace_all("_", " ")
}

display_predictors <- function(predictors) {
  predictors |>
    str_replace_all("b_", "") |>
    label_caps() |>
    str_replace("Mlu", "MLU-w") |>
    str_replace("Length phon", "Length in phonemes") |>
    str_replace("Freq", "Frequency") |>
    str_replace("Cd", "Contextual diversity") |>
    str_replace("Mdd", "MDD")
}

term_fct <- c(
  "Frequency", "Burstiness", "Context diversity",
  "Concreteness", "Babiness", "Sensorimotor", "Emotionality",
  "Length in phonemes", "Phon neighbours",
  "N features", "Form entropy", "N morphemes",
  "Subcat entropy", "MDD", "MLU-w"
)

standardise_terms <- function(df) {
  df |>
    mutate(
      term = term |>
        str_replace("neighbor", "neighbour") |>
        factor() |>
        fct_relabel(display_predictors) |>
        fct_relevel(term_fct) |>
        fct_rev(),
      term_cat = fct_collapse(
        term,
        Distributional = c("Frequency", "Burstiness", "Context diversity"),
        Phonological = c("Length in phonemes", "Phon neighbours"),
        Morphological = c("N features", "Form entropy", "N morphemes"),
        Syntactic = c("MLU-w", "Subcat entropy", "MDD"),
        Semantic = c("Concreteness", "Babiness", "Sensorimotor",
                     "Emotionality", "Socialness")) |>
        fct_relevel(c("Phonological", "Morphological",
                      "Syntactic", "Semantic", "Distributional"))
    )
}

theme_mikabr <- function(base_size = 14, base_family = "Open Sans") {
  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family),
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank())
  )
}

# Posterior helpers

summarise_draws <- function(grouped_draws) {
  grouped_draws |>
    summarise(map = map_estimate(estimate)$MAP_Estimate,
              eap = mean(estimate),
              sd = sd(estimate),
              hdi = hdi(estimate),
              eti = eti(estimate),
              pct_rope = sum(-.1 <= estimate & estimate <= .1) / n()) |>
    mutate(hdi.lower = hdi$CI_low,
           hdi.upper = hdi$CI_high,
           eti.lower = eti$CI_low,
           eti.upper = eti$CI_high,
           reliability = sign(hdi.lower) == sign(hdi.upper)) |>
    select(-hdi, -eti)
}

