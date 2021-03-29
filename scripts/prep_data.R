##................................... 
## data prep -- scaling, imputation, etc
##...................................

library(here)
library(glue)
library(broom)
library(tidyverse)

# HELPER FUNCTIONS --------------

#Residualize - do each of these get their own functions?
get_final_freq <- function(uni_childes) {
  return(lm(final_freq ~ requency, data = uni_childes)$residuals)
}
get_solo_freq <- function(uni_childes) {
  return(lm(final_freq ~ requency, data = uni_childes)$residuals)
}

fit_predictor <- function(pred, d, pred_sources) {
  xs <- pred_sources %>% discard(~pred %in% .x) %>% unlist()
  x_str <- xs %>% paste(collapse = " + ")
  lm(as.formula(glue("{pred} ~ {x_str}")), data = d) %>%
    augment(newdata = d) %>%
    select(uni_lemma, lexical_category, .fitted)
}

get_missing_data <- function(lang_data, predictors) {
  missing <- lang_data %>%
    pivot_longer(cols = !!predictors, names_to = "predictor", values_to = "value") %>%
    mutate(missing = is.na(value)) %>%
    select(-value) %>%
    pivot_wider(names_from = predictor, values_from = missing)
  return(missing)
}

#start with predictors with the fewest NAs
#create a list that can be iterated over
get_predictor_order <- function(lang_data, predictors, max_steps) {
  predictor_order <- lang_data %>%
    pivot_longer(cols = !!predictors, names_to = "predictor", values_to = "value") %>%
    group_by(predictor) %>%
    summarise(num_na = sum(is.na(value))) %>%
    filter(num_na != 0) %>%
    arrange(num_na) %>%
    pull(predictor)
  
  num_repeats = max_steps %/% length(predictor_order) + 1
  return(rep(predictor_order, num_repeats)[0:max_steps])
}

get_imputation_seed <- function(lang_data, predictors){
  imputation_data <- lang_data %>%
    mutate_at(vars(!!predictors),
              funs(as.numeric(Hmisc::impute(., fun = "random"))))
  return(imputation_data)
}

#takes in a predictor from the list of predictors, pulls the imputation data at current status
#returns a new imputation data

do_iterate_imputation <- function(pred_sources, imputation_data, missing) {
  prediction_list <- unlist(pred_sources)
  #iterates through the predictor list for that language
  for (pred in prediction_list) {
  imputation_fits <- fit_predictor(pred, imputation_data, pred_sources)
  imputation_data <- missing %>%
    select(uni_lemma, lexical_category, !!pred) %>%
    rename(missing = !!pred) %>%
    right_join(imputation_data) %>%
    left_join(imputation_fits) %>%
    #if the value is missing, replace it with the new value
    mutate_at(vars(pred), funs(if_else(is.na(missing), .fitted, .))) %>%
    select(-.fitted, -missing) }
  return(imputation_data) 
}


do_lang_imputation <- function(language, data, pred_sources, max_steps) {
  #if all the predictors are from one source, fix the pred_sources
  if (length(pred_sources) == 1) {pred_sources = unlist(pred_sources) }
  predictors <- unlist(pred_sources)
  print(glue("Imputing {language} with {max_steps} steps..."))
  predictor_list <- get_predictor_order(data, predictors, max_steps)
  #What do we do if a language is missing a predictor entirely? 
  missing_data <- get_missing_data(data, predictors)
  imputed_data <- get_imputation_seed(data, predictors)
  imputed_data <- do_iterate_imputation(pred_sources, imputed_data, missing_data)
  return(imputed_data)
}

#takes in the data in the format 
do_full_imputation <- function(model_data, pred_sources, max_steps) {
  #restrict to the sources in pred_sources
  #TODO: Catch cases where a predictor in the predictor set isnt in the data
  nested_data <- model_data %>% 
    select(language, uni_lemma, lexical_category, !!unlist(pred_sources)) %>%
    distinct() %>%
    group_by(language) %>% 
    nest()
  
  imputed_data <- nested_data %>% mutate(imputed = map2(language, data, 
                                        ~do_lang_imputation(.x, .y, 
                                                            pred_sources, max_steps)))
  return(imputed_data)
}

#TEST/EXAMPLE CASE ------

load(here("data/temp_saved_data/uni_joined.RData")) #this doesnt totally match the eng_data

#The data we're reading in as predictors should already have this cleaned
test_data <- uni_joined %>%
  #select out just the by lexical item data
  select(-c(age, num_true, num_false, prop)) %>%
  distinct() %>%
  #pull out categories from classes
  mutate(lexical_category = if_else(str_detect(lexical_classes, ","), "other", lexical_classes),
         # collapse v, adj, adv into one category
         lexical_category = lexical_category %>% as_factor() %>% 
           fct_collapse("predicates" = c("verbs", "adjectives", "adverbs"))) %>%
  select(-lexical_classes)

#1. Specify predictors
pred_sources <- list(
  c("frequency", "MLU", "final_frequency", "solo_frequency"),
  c("valence", "arousal"), 
  "concreteness", "babiness", "num_phons"
)

test_imputed_data <- test_data %>% do_full_imputation(pred_sources, 20)

#TEST 2, Adding new predictors (test w/ entropy)
entropy_data <- read_csv(here("data/temp_saved_data/type_entropies.csv"))
word_map <- read_csv(here("data/temp_saved_data/WSWG_50percentproducing_cleaned.csv")) %>% 
  rename(wordtype = type) %>% rename(word_type = wordtype)
entropy_data <- entropy_data %>% left_join(word_map) %>% mutate(language = "English (American)")

test_data_two <- test_data %>%left_join(entropy_data %>% select(language, uni_lemma, child_entropy, adult_entropy))

#1. Specify predictors
pred_sources_two <- list(
  c("frequency", "MLU", "final_frequency", "solo_frequency"),
  c("valence", "arousal"), 
  c("adult_entropy", "child_entropy"),
  "concreteness", "babiness", "num_phons"
)

test_imputed_data_entropy <- test_data_two %>% filter(language == "English (American)") %>% do_full_imputation(pred_sources_two, 20)

#2.  Add back in wordbank data
uni_model_data <- model_data_imputed  %>%
  unnest() %>%
  group_by(language) %>%
  mutate_at(vars(predictors), funs(as.numeric(scale(.)))) %>%
  right_join(uni_joined %>% select(language, measure, uni_lemma, age, num_true,
                                   num_false)) %>%
  group_by(language, measure) %>%
  mutate(unscaled_age = age, age = scale(age),
         total = as.double(num_true + num_false), prop = num_true / total)