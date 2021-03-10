library(tidyverse)
library(glue)
library(wordbankr)

get_inst_admins <- function(language, form, exclude_longitudinal = TRUE) {
  
  admins <- get_administration_data(language = language, form = form,
                                    original_ids = TRUE)
  
  if (exclude_longitudinal) {
    # take earliest administration for any child with multiple administrations
    admins <- admins %>%
      mutate(source_group = str_replace(source_name, " \\(.*\\)", "")) %>%
      group_by(source_group, original_id) %>%
      slice_min(age, with_ties = FALSE) %>%
      ungroup()
    
  }
  
  admins %>% select(language, form, age, data_id)
}

get_inst_words <- function(language, form) {
  get_item_data(language = language, form = form) %>%
    filter(type == "word") %>%
    select(language, form, lexical_class, category, uni_lemma, definition,
           item_id, num_item_id)
}

get_inst_data <- function(language, form, admins, items) {
  print(glue("Getting data for {language} {form}..."))
  
  get_instrument_data(language = language,
                      form = form,
                      items = items$item_id,
                      administrations = admins,
                      iteminfo = items) %>%
    mutate(produces = !is.na(value) & value == "produces",
           understands = !is.na(value) &
             (value == "understands" | value == "produces")) %>%
    select(-value) %>%
    pivot_longer(names_to = "measure", values_to = "value",
                 cols = c(produces, understands)) %>%
    filter(measure == "produces" | form == "WG") %>%
    select(-item_id, -num_item_id)
}

collapse_inst_data <- function(inst_data) {
  print(glue("Wrangling data for {unique(inst_data$language)} {unique(inst_data$form)}..."))
  
  inst_uni_lemmas <- inst_data %>%
    distinct(measure, lexical_class, category, uni_lemma, definition) %>%
    group_by(uni_lemma) %>%
    nest(items = c(lexical_class, category, definition))
  
  inst_data %>%
    filter(!is.na(value)) %>%
    # for each child and uni_lemma, collapse across items
    group_by(language, form, measure, uni_lemma, age, data_id) %>%
    summarise(uni_value = any(value)) %>%
    # for each age and uni_lemma, collapse across children
    group_by(language, form, measure, uni_lemma, age) %>%
    summarise(num_true = sum(uni_value),
              num_false = n() - num_true,
              prop = mean(uni_value)) %>%
    ungroup() %>%
    left_join(inst_uni_lemmas)
  
}

create_inst_data <- function(language, form) {
  inst_label <- paste(language, form, sep = "_") %>% str_replace(" ", "_") %>% str_to_lower()
  inst_admins <- get_inst_admins(language, form)
  inst_words <- get_inst_words(language, form)
  inst_data <- get_inst_data(language, form, inst_admins, inst_words)
  inst_props <- collapse_inst_data(inst_data)
  save(inst_props, file = glue("data/wordbank/{inst_label}.RData"))
}

# create_inst_data("English (American)", "WG")
