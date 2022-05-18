library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)

############################# US

mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Romance/mor.rds')
mor <-  dplyr::filter(mor, grepl("Italian",transcript))
mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Romance/mk.rds')
mk <-dplyr::filter(mk, grepl("Italian",transcript))
w <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Romance/w.rds')
w <-dplyr::filter(w, grepl("Italian",transcript))
word <- w %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Romance/Italian/", "", transcript)) %>% #Eng-NA
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "ita", token="*") #role_exclude = "Target_Child"

base <- mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Romance/Italian/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

######################################## functions

summ_morph<- function(morphology){

  new_morphology <- do.call("rbind", list( morphology)) %>%
    select(id, stem, pos, prefix, affix, affix_type, gloss, corpus_name, suffix, utterance_id)

  new_morphology_ <- new_morphology %>%
    group_by(id, utterance_id, corpus_name, gloss) %>%
    summarise(stem = paste(sort(unique(stem)),collapse=", "),
              affix = paste(sort(unique(affix)),collapse=", "),
              affix_type = paste(sort(unique(affix_type)),collapse=", "),
              gloss = paste(sort(unique(gloss)),collapse=", "),
              prefix = paste(sort(unique(prefix)),collapse=", "),
              suffix = paste(sort(unique(suffix)),collapse=", "),
              pos = paste(sort(unique(pos)),collapse=", "))
  print(new_morphology_)
}


match_morph_to_childes <- function(name_corpus, name_collection){

  transcript <- get_transcripts(corpus=name_corpus)

  w <- word  %>% filter(corpus_name==name_corpus)
  w <- w[order(w$transcript), ]

  corpus <- childes %>% filter(corpus_name==name_corpus) %>% left_join(transcript)
  corpus <- corpus %>%
    mutate(filename = gsub(name_collection, "", filename)) %>%
    rename(gloss_childes=gloss)
  corpus <- corpus[order(corpus$filename), ]

  a<-intersect(unique(w$transcript), unique(corpus$filename))

  corpus <- corpus %>%
    filter(filename %in% a)
  w <- w  %>%
    filter(transcript %in% a)

  corpus <- tibble::rowid_to_column(corpus, "matching_index")
  w <- tibble::rowid_to_column(w, "matching_index")

  language <- base %>%  filter(corpus_name==name_corpus) %>%   ###remove extra sessions
    filter(transcript %in% a)

  corpus_childes <- w %>%
    rename(morph_id=id ) %>%
    select(-corpus_name ) %>%
    distinct() %>%
    left_join(corpus)

  final <- language %>% left_join(corpus_childes)

  print(final)
}

final_tonelli<-match_morph_to_childes( "Tonelli", "Italian/")
tonelli_morphology <- summ_morph(final_tonelli)

########################################

#spa_morphology <- do.call("rbind", list(serrasole_morphology, remedi_morphology, colmex_morphology, hess_morphology, grerli_morphology, marrero_morphology, fernaguado_morphology, aguirre_morphology))

ita_morphology <- tonelli_morphology

tokens_ita <- get_tokens(language = "ita", token="*", role_exclude = "Target_Child")

tokens_ita_ <- tokens_ita %>%
  left_join(ita_morphology)

tokens_ita__ <- tokens_ita_ %>%
  filter(!is.na(gloss_m)) %>%
  rename(token_id=id) %>%
  mutate(affix_m = lapply(strsplit(as.character(affix_m),split=','),trimws))

saveRDS(tokens_ita__, "/Users/loukatou/Documents/aoa-pipeline/data/childes/morph_ita.rds")






