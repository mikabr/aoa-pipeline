##................................... 
## data prep -- scaling, imputation, etc
##...................................

library(here)
library(glue)
library(broom)
library(tidyverse)
#TODO: Used anywhere?
#library(langcog)

#TODO: We might want just the item level data, instead of the full data with wordbank bits
#TODO: Will childes.R output this, or will we need to do some combining on the fly for things like concreteness
load(here("data/temp_saved_data/uni_joined.RData"))

#TODO: Where will user specified predictors live? Should this be a top level specification or at each step (like this)?

#Set predictors
#MQ: Whats the difference between predictors and pred sources

predictors <- c("frequency", "MLU", "final_frequency", "solo_frequency",
                "num_phons", "concreteness", "valence", "arousal", "babiness")

#MQ: Why's this a list in a list?
pred_sources <- list(
  c("frequency", "MLU", "final_frequency", "solo_frequency"),
  c("valence", "arousal"),
  "concreteness", "babiness", "num_phons"
)

#TODO: used anywhere?
#.alpha <- 0.05
set.seed(1000)

# HELPER FUNCTIONS --------------

fit_predictor <- function(pred, d) {
  xs <- pred_sources %>% discard(~pred %in% .x) %>% unlist()
  x_str <- xs %>% paste(collapse = " + ")
  lm(as.formula(glue("{pred} ~ {x_str}")), data = d) %>%
    augment(newdata = d) %>%
    select(uni_lemma, lexical_category, .fitted)
}

get_missing_data <- function(lang_data) {
  missing <- lang_data %>%
    pivot_longer(cols = !!predictors, names_to = "predictor", values_to = "value") %>%
    mutate(missing = is.na(value)) %>%
    select(-value) %>%
    pivot_wider(names_from = predictor, values_from = missing)
  return(missing)
}

#start with predictors with the fewest NAs
#create a list that can be iterated over
get_predictor_order <- function(lang_data, max_steps) {
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

get_imputation_seed <- function(lang_data){
  imputation_data <- lang_data %>%
    mutate_at(vars(!!predictors),
              funs(as.numeric(Hmisc::impute(., fun = "random"))))
  return(imputation_data)
}

#takes in a predictor from the list of predictors, pulls the imputation data at current status
#returns a new imputation data
do_impute <- function(prediction_list, imputation_data, missing) {
  #iterates through the predictor list for that language
  for (pred in prediction_list) {
  imputation_fits <- fit_predictor(pred, imputation_data)
  
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

# 0. Drop the subject data? Maybe the subject data and item data w/ predictors should be separate
model_data <- uni_joined %>%
  #select out just the by lexical item data
  select(language, uni_lemma, lexical_classes, !!predictors) %>%
  distinct() %>%
  #pull out categories from classes
  mutate(lexical_category = if_else(str_detect(lexical_classes, ","), "other", lexical_classes),
         # collapse v, adj, adv into one category
         lexical_category = lexical_category %>% #as_factor() %>% #MQ: Whats the point of as_factor here?
           fct_collapse("predicates" = c("verbs", "adjectives", "adverbs"))) %>%
  select(-lexical_classes) %>%
  group_by(language) %>%
  mutate_at(vars(!!predictors), funs(as.numeric(scale(.)))) %>% #MQ: This was commented, why? 
  nest()

# 1. Iterate predictors over the predictors to impute the data

max_steps = 20
model_data_imputed <- model_data %>%
  mutate(missing = map(data, get_missing_data),
         predictor_order = map(data, get_predictor_order, max_steps),
         #start with one imputation step as the start (is this really needed?)
         imputed_data = map(data, get_imputation_seed)) %>%
  #MQ: need help making this work, i just want to use a mapply, tbh?
  rowwise() %>%
  summarize(imputed_data = do_impute(predictor_order, imputed_data, missing))

# 2. Add back in wordbank data
uni_model_data <- model_data_imputed  %>%
  unnest() %>%
  group_by(language) %>%
  mutate_at(vars(predictors), funs(as.numeric(scale(.)))) %>%
  right_join(uni_joined %>% select(language, measure, uni_lemma, age, num_true,
                                   num_false)) %>%
  group_by(language, measure) %>%
  mutate(unscaled_age = age, age = scale(age),
         total = as.double(num_true + num_false), prop = num_true / total)