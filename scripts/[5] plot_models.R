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
  "Frequency", "Context diversity", "Burstiness",
  "Concreteness", "Sensorimotor", "Babiness", "Emotionality",
  "Length in phonemes", "Phon neighbours",
  "N features", "N morphemes", "Form entropy",
  "MLU-w", "MDD", "Subcat entropy"
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

median_cl_boot <- function(x, conf = 0.95) {
  b_median <- function(data, indices) median(data[indices])
  boot_res <- boot(data = x, statistic = b_median, R = 1000)
  boot_ci <- boot.ci(boot_res, conf = conf, type = "perc")

  tibble(
    y = median(x),
    ymin = boot_ci$percent[4],
    ymax = boot_ci$percent[5]
  )
}

corpus_correlation_alternative <- function(main_coefs, morph_complexity,
                                           n_boot = 1000) {
  wide <- main_coefs |>
    mutate(corpus = childes_corpus(language)) |>
    filter(!is.na(corpus), corpus != "") |>
    group_by(corpus, term) |>
    summarise(estimate = mean(estimate), .groups = "drop") |>
    left_join(
      morph_complexity |>
        mutate(corpus = childes_corpus(language)) |>
        distinct(corpus, language_family),
      by = "corpus"
    ) |>
    filter(!is.na(language_family)) |>
    select(corpus, language_family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate)

  families <- wide$language_family
  mat <- wide |> select(-corpus, -language_family) |> as.matrix()
  pair_r <- cor(t(mat), use = "pairwise.complete.obs")
  fam_names <- unique(families)

  weighted_mean_r <- function(counts) {
    c_i <- as.numeric(counts[families])
    n <- length(c_i)
    num <- 0
    den <- 0
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        r <- pair_r[i, j]
        if (is.na(r)) next
        w <- if (families[i] == families[j]) c_i[i] else c_i[i] * c_i[j]
        if (is.na(w) || w == 0) next
        num <- num + w * r
        den <- den + w
      }
    }
    if (den == 0) NA_real_ else num / den
  }

  ones <- setNames(rep(1, length(fam_names)), fam_names)
  boots <- map_dbl(seq_len(n_boot), \(b) {
    drawn <- sample(fam_names, length(fam_names), replace = TRUE)
    counts <- table(factor(drawn, levels = fam_names))
    weighted_mean_r(counts)
  })
  tibble(
    estimate = weighted_mean_r(ones),
    ci.lb = unname(quantile(boots, 0.025, na.rm = TRUE)),
    ci.ub = unname(quantile(boots, 0.975, na.rm = TRUE))
  )
}

