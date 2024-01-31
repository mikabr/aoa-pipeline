# Load data
aoa_models_lexcat <- readRDS("outputs/aoa_models_lexcat_output.rds")
aoa_models <- readRDS("outputs/aoa_models_output.rds")
aoa_predictor_data <- readRDS("outputs/aoa_predictor_data.rds")
mc_mods <- readRDS("outputs/mc_mods.rds")
morph_complexity <- read_csv("outputs/morph_complexity.csv")
descriptives <- read_csv("outputs/descriptives.csv")

# Extract coefs
aoa_coefs <- aoa_models_lexcat |>
  filter(measure == "produces") |>
  select(language, coefs) |>
  unnest(coefs) |>
  filter(Parameter != "b_Intercept") |>
  mutate(
    effect = if_else(str_detect(Parameter, ":"), "interaction", "main"),
    lexical_category = str_extract(Parameter, "lexical_category[0-9]"),
    term = if_else(effect == "interaction",
                   str_remove(Parameter, ":?lexical_category[0-9]:?"),
                   Parameter) |>
      str_replace("neighbor", "neighbour"),
    term = term |>
      factor() |>
      fct_relabel(display_predictors) |>
      fct_relevel(term_fct) |>
      fct_rev(),
    language = factor(language, levels = target_langs),
    term_cat = fct_collapse(term,
                            Morphological = c("N features", "Form entropy", "N morphemes"),
                            Syntactic = c("MLU-w", "Subcat entropy"),
                            Phonological = c("Length in phonemes", "Phon neighbours"),
                            Other = c("Frequency", "Concreteness", "Babiness")) |>
      fct_rev() |>
      fct_shift(-2)) |>
  rename(estimate = MAP,
         conf.low = CI_low,
         conf.high = CI_high)

# Summary stats
n_by_lexcat <- aoa_predictor_data |>
  group_by(language, lexical_category) |>
  summarise(n = n())

n_by_lang <- n_by_lexcat |>
  group_by(language) |>
  summarise(n = sum(n))

# VIFs
vifs <- aoa_models |>
  filter(measure == "produces") |>
  select(language, vifs) |>
  unnest(vifs)

# Wrangle for plotting
plot_coefs <- aoa_coefs |>
  filter(is.na(lexical_category),
         !is.na(term)) |>
  left_join(n_by_lang, by = "language") |>
  mutate(signif = ifelse(conf.low < 0 & conf.high > 0,
                         "Not significant", "Significant"),
         language = factor(language, levels = target_langs))

lc_plot <- aoa_coefs |>
  select(language, term, estimate, lexical_category, term_cat) |>
  pivot_wider(names_from = lexical_category, values_from = estimate) |>
  mutate(predicates = `NA` + lexical_category1,
         function_words = `NA` + lexical_category2,
         nouns = `NA` - lexical_category1 - lexical_category2) |>
  select(-c(`NA`, lexical_category1, lexical_category2)) |>
  pivot_longer(cols = c(predicates, function_words, nouns),
               names_to = "lexical_category",
               values_to = "estimate") |>
  filter(!is.na(estimate),
         !is.na(term)) |>
  left_join(n_by_lexcat, by = c("language", "lexical_category")) |>
  mutate(language = factor(language, levels = target_langs),
         lexical_category = factor(lexical_category, levels =
                                     c("nouns", "predicates", "function_words")) |>
           fct_relabel(label_caps))

# Dendrogram
dd_coefs <- aoa_coefs |>
  filter(is.na(lexical_category)) |>
  select(language, term, lexical_category, estimate) |>
  pivot_wider(names_from = c(term, lexical_category),
              values_from = estimate) |>
  left_join(morph_complexity |>
              select(language, iso_unique)) |>
  mutate(language = factor(language, levels = target_langs)) |>
  arrange(language)

dd_matrix <- dd_coefs |>
  select(-language, -iso_unique) |>
  as.matrix() |>
  `rownames<-`(dd_coefs$iso_unique)

dd_cor <- cor(t(dd_matrix), use = "pairwise.complete.obs")

dd_cor_long <- dd_cor |>
  as_tibble(rownames = "language_1") |>
  pivot_longer(cols = -language_1, names_to = "language_2") |>
  left_join(morph_complexity |> select(iso_unique, family_1 = language_family),
            by = c("language_1" = "iso_unique")) |>
  left_join(morph_complexity |> select(iso_unique, family_2 = language_family),
            by = c("language_2" = "iso_unique"))

dd_cor_gen <- dd_cor_long |>
  filter(language_1 != language_2) |>
  group_by(language_1) |>
  summarise(estimate = mean(value),
            ci.lb = mean(value) - 0.95 * sd(value) / sqrt(n()),
            ci.ub = mean(value) + 0.95 * sd(value) / sqrt(n()))

dd_cor_fam <- dd_cor_long |>
  filter(language_1 != language_2,
         family_1 == family_2) |>
  group_by(language_1) |>
  summarise(estimate = mean(value),
            ci.lb = mean(value) - 0.95 * sd(value) / sqrt(n()),
            ci.ub = mean(value) + 0.95 * sd(value) / sqrt(n()))

dd_cor_rand <- lapply(seq(100), \(i) {
  apply(dd_matrix, 1, sample) |>
    cor(use = "pairwise.complete.obs") |>
    as_tibble(rownames = "language_1") |>
    pivot_longer(cols = -language_1, names_to = "language_2") |>
    filter(language_1 != language_2) |>
    mutate(sim = i)}) |>
  bind_rows() |>
  group_by(language_1) |>
  summarise(estimate = mean(value),
            ci.lb = mean(value) - 0.95 * sd(value) / sqrt(n()),
            ci.ub = mean(value) + 0.95 * sd(value) / sqrt(n()))

dd_cor_all <- dd_cor_gen |>
  left_join(dd_cor_fam, by = "language_1", suffix = c("", "_fam")) |>
  left_join(dd_cor_rand, by = "language_1", suffix = c("_gen", "_rand")) |>
  rename(iso_unique = language_1) |>
  left_join(morph_complexity |> select(language, iso_unique, language_family), by = "iso_unique") |>
  mutate(language = factor(language, target_langs) |> fct_rev()) |>
  arrange(desc(language)) |>
  mutate(language_family = as_factor(language_family))

# Morphosyntax
mc_vals <- expand_grid(measure = c("production", "comprehension"),
                       ms_term = ms_terms,
                       lexical_category = lexcats) |>
  mutate(mc_data = map(mc_mods, tidy)) |>
  unnest(mc_data) |>
  mutate(signif = p.value < .05) |>
  filter(measure == "production")

mlu_pred <- lc_plot |>
  filter(term == "MLU-w",
         lexical_category == "Predicates") |>
  left_join(morph_complexity |>
              select(language, iso_unique, morph_complexity)) |>
  mutate(language = factor(language, levels = target_langs))
