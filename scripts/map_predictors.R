library(here)
library(tidyverse)

eng_data <- readRDS(here("data/wordbank/english_(american).rds"))

# uni_lemmas <- eng_data %>% unnest(cols = "items") %>%
#   select(-c(measure, age, num_true, total)) %>%
#   distinct()

uni_lemmas <- eng_data %>% distinct(language, uni_lemma)

#takes in a csv of predictor measures that may or may not map to uni_lemmas and maps them
map_predictor <- function(predictor_csv, replacement_csv, variable_mapping) {
  predictor_data <- read_csv(predictor_csv)
  replacements <- read_csv(replacement_csv)
  predictors <- discard(names(variable_mapping), ~. == "word")
  
  #clean the predictors
  renamed_predictors <- predictor_data %>% 
    rename(all_of(variable_mapping)) %>% 
    select(names(variable_mapping)) %>%
    group_by(word) %>%
    #in case a word appears in the list twice
    summarize(across({{ predictors }}, mean))
  
  #TODO: What do we do about things like "chips" and "can (auxiliary)"
  uni_lemma_predictors <- uni_lemmas %>% left_join(replacements) %>%
    #if there is a hand written replacement, use the replacement
    #otherwise default to the cleaned uni_lemma
    mutate(word = case_when(!is.na(replacement) & replacement != "" ~ replacement,
                            TRUE ~ str_replace(uni_lemma, "\\s*\\([^\\)]+\\)", ""))) %>%
    select(-replacement) %>%
    left_join(renamed_predictors) %>%
    select(-word) %>%
    group_by(language, uni_lemma) %>%
    summarize(across({{ predictors }}, mean))
  
  return(uni_lemma_predictors)
}

babiness_csv <- here("data/predictors/babiness/babiness.csv")
babiness_replace_csv <- here("data/predictors/babiness/babiness_replace.csv")
babiness_map <- c(word = "word", iconicity = "rating", babiness = "babyAVG")

baby_unilemma <- map_predictor(babiness_csv, babiness_replace_csv, babiness_map)

valence_csv <- here("data/predictors/valence/valence.csv")
valence_replace_csv <- here("data/predictors/valence/valence_replace.csv")
valence_mapping <- c(word = "Word", valence = "V.Mean.Sum", arousal = "A.Mean.Sum")

valence_unilemma <- map_predictor(valence_csv, valence_replace_csv, valence_mapping)

concreteness_csv <- here("data/predictors/concreteness/concreteness.csv")
concreteness_replace_csv <- here("data/predictors/concreteness/concreteness_replace.csv")
concreteness_map <- c(word = "Word", concreteness = "Conc.M")

conctreteness_unilemma <- map_predictor(concreteness_csv, concreteness_replace_csv, concreteness_map)

uni_joined <- eng_data %>% left_join(baby_unilemma) %>% left_join(valence_unilemma) %>% left_join(conctreteness_unilemma)
