library(reticulate)
np <- import("numpy")

load_utt_embed_data <- function(lang, corpus_args = default_corpus_args) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- here(childes_path, glue("utterance_embeddings_{childes_lang}.npy"))
  if(file.exists(file_p)) {
    message(glue("Loading cached utterance embed data for {lang}."))
    embed <- np$load(file_p)
  } else {
    # temporary; maybe can find a way to use reticulate to do this
    stop(glue("No cached utterance embed data for {lang}. Please run embed.py on the relevant language to generate embeddings."))
  }
  embed
}

load_lemma_embed_data <- function(lang, corpus_args = default_corpus_args) {
  childes_lang <- convert_lang_childes(lang)
  file_p <- here(childes_path, glue("lemma_embeddings_{childes_lang}.rds"))
  if(file.exists(file_p)) {
    message(glue("Loading cached lemma embed data for {lang}."))
    embed <- readRDS(file_p)
  } else {
    # temporary; maybe can find a way to use reticulate to do this
    stop(glue("No cached lemma embed data for {lang}. Please run embed.py on the relevant language to generate embeddings."))
  }
  embed
}

compute_semantic_diversity <- function(embed_data) {
  print("Computing semantic diversity...")

  centroids <- metric_data |>
    group_by(lemma) |>
    summarise(mean_utterance_embed = list(reduce(utterance_embed, `+`) / length(utterance_embed)))

  distances <- metric_data |>
    left_join(centroids, by = join_by(lemma)) |>
    mutate(utt_embed_dist_abs = map2(utterance_embed, mean_utterance_embed, \(e, m) e - m),
           utt_embed_dist_eucl = sqrt(rowSums(do.call(rbind, utt_embed_dist_abs) ^ 2)))

  semantic_diversity <- distances |>
    group_by(token) |>
    summarise(semantic_diversity = mean(utt_embed_dist_eucl))

  # semantic_diversity_tokens <- metric_data |>
  #   select(token, lemma) |>
  #   distinct() |>
  #   left_join(semantic_diversity, by = join_by(lemma)) |>
  #   select(-lemma) |>
  #   group_by(token) |>
  #   summarise(semantic_diversity = mean(semantic_diversity))
}

compute_semantic_support <- function(embed_data) {
  print("Computing semantic support...")

  cosine_sim <- function(x, y) {
    sum(x * y) / (sqrt(sum(x ^ 2)) * sqrt(sum(y ^ 2)))
  }

  distances <- metric_data |>
    mutate(semantic_support = pmap_dbl(list(utterance_embed, lemma_embed, utterance_length),
                                       \(ue, le, ul) {
                                         # context_embed is the amount of contextual info without the lemma itself
                                         context_embed <- (ue - (le / ul))
                                         # semantic_support is the cosine similarity between the context_embed and the lemma_embed
                                         cosine_sim <- cosine_sim(context_embed, le)
                                       }))

  semantic_support <- distances |>
    filter(!is.na(lemma)) |>
    group_by(token) |>
    summarise(semantic_support = mean(semantic_support, na.rm = TRUE))
}
