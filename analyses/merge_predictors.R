library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)


french_mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/French/mor.rds')
french_mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/French/mk.rds')
french_word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/French/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/French/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "fra", token="*") #role_exclude = "Target_Child"

french_base <- french_mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(french_mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/French/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

#########################
#Champaud

transcript <- get_transcripts(corpus="Champaud")

french_w <- french_word  %>% filter(corpus_name=="Champaud")
french_w <- french_w[order(french_w$transcript), ] %>%   ###remove extra sessions
  filter(transcript != "Champaud/011003.xml" )
french_w <- tibble::rowid_to_column(french_w, "matching_index")

#french_w$gloss_ann <- ifelse(french_w$gloss > champaud$gloss, 'DIFFERENT',
#                      ifelse(french_w$transcript == champaud$filename, '', 'DIFFERENT'))  ###check matching

champaud <- childes %>% filter(corpus_name=="Champaud") %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename)) %>%
  rename(gloss_childes=gloss)
champaud <- champaud[order(champaud$filename), ]
champaud <- tibble::rowid_to_column(champaud, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Champaud")

champaud_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(champaud)

final_champaud <- french %>% left_join(champaud_childes)

#############################################

transcript <- get_transcripts(corpus="Palasis")

french_w <- french_word  %>% filter(corpus_name=="Palasis")
french_w <- french_w[order(french_w$transcript), ]  %>% filter(!incomplete_type =="omission") #%>%   ###remove extra sessions
french_w <- french_w[-c(8175), ]
french_w <- french_w[-c(18997), ]
french_w <- french_w[-c(21684), ]
french_w <- french_w[-c(37742), ]
french_w <- french_w[-c(38791), ]
french_w <- french_w[-c(38891), ]
french_w <- french_w[-c(40151), ]
french_w <- french_w[-c(42444), ]
french_w <- french_w[-c(42642), ]
french_w <- french_w[-c(43729), ]
french_w <- french_w[-c(45469), ]
french_w <- french_w[-c(51367), ]
french_w <- french_w[-c(68520), ]
french_w <- french_w[-c(69681), ]
french_w <- french_w[-c(70620), ]
french_w <- french_w[-c(70648), ]
french_w <- french_w[-c(77611), ]
french_w <- french_w[-c(78959), ]
french_w <- french_w[-c(79370), ]
french_w <- french_w[-c(83746), ]
french_w <- french_w[-c(83765), ]
french_w <- french_w[-c(89856), ]
french_w <- french_w[-c(89978), ]
french_w <- french_w[-c(90521), ]
french_w <- french_w[-c(90569), ]
french_w <- french_w[-c(90545), ]
french_w <- french_w[-c(91426), ]
french_w <- french_w[-c(95835), ]
french_w <- french_w[-c(97627), ]
french_w <- french_w[-c(108013), ]
french_w <- french_w[-c(108119), ]
french_w <- french_w[-c(108136), ]
french_w <- french_w[-c(108231, 108241), ]
french_w <- french_w[-c(108440), ]
french_w <- french_w[-c(109958), ]
french_w <- french_w[-c(109964), ]
french_w <- french_w[-c(109968), ]
french_w <- french_w[-c(109972), ]
french_w <- french_w[-c(122201), ]
french_w <- french_w[-c(122484), ]
french_w <- french_w[-c(122612), ]
french_w <- french_w[-c(122617), ]
french_w <- french_w[-c(126941), ]
french_w <- french_w[-c(126949), ]
french_w <- french_w[-c(127508), ]
french_w <- french_w[-c(130459), ]
french_w <- french_w[-c(131074), ]
french_w <- french_w[-c(131105), ]
french_w <- french_w[-c(131174), ]
french_w <- french_w[-c(131196), ]
french_w <- french_w[-c(131215), ]
french_w <- french_w[-c(131287), ]
french_w <- french_w[-c(131299), ]
french_w <- french_w[-c(131928), ]
french_w <- french_w[-c(147873), ]
french_w <- french_w[-c(164747), ]
french_w <- french_w[-c(174570), ]
french_w <- french_w[-c(208072), ]
french_w <- french_w[-c(213959), ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

palasis <- childes %>% filter(corpus_name=="Palasis") %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
palasis <- palasis[order(palasis$filename), ]
palasis <- tibble::rowid_to_column(palasis, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Palasis")

#palasis <- palasis[-c(268213), ]

#french_w$gloss_ann <- ifelse(french_w$gloss > palasis$gloss, 'DIFFERENT', ifelse(french_w$transcript == palasis$filename, '', 'DIFFERENT'))  ###check matching

palasis_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(palasis)

final_palasis <- french %>% left_join(palasis_childes)

########################################

transcript <- get_transcripts(corpus="Pauline")

french_w <- french_word  %>% filter(corpus_name=="Pauline")
french_w <- french_w[order(french_w$transcript), ] #%>%   ###remove extra sessions

french_w <- french_w[-c(3874), ]
french_w <- french_w[-c(16244), ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

pauline <- childes %>% filter(corpus_name=="Pauline") %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
pauline <- pauline[order(pauline$filename), ]
pauline <- tibble::rowid_to_column(pauline, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Pauline")

pauline_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(pauline)

final_pauline <- french %>% left_join(pauline_childes)


########################################

transcript <- get_transcripts(corpus="York")

french_w <- french_word  %>% filter(corpus_name=="York")
french_w <- french_w[order(french_w$transcript), ]  %>% filter(!incomplete_type =="omission") #   %>%   ###remove extra sessions

#fren <- french_w[french_w$gloss!=york$gloss_childes, ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

york <- get_tokens(corpus="York", token="*")

york <- york %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
york <- york[order(york$filename), ]
york <- tibble::rowid_to_column(york, "matching_index")

french <- french_base%>%
  filter(corpus_name=="York")

#french_w$gloss_ann <- ifelse(french_w$gloss > york$gloss_childes, 'DIFFERENT', ifelse(french_w$transcript == york$filename, '', 'DIFFERENT'))  ###check matching

york_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(york)

final_york <- french %>% left_join(york_childes)

########################################

transcript <- get_transcripts(corpus="Geneva")

french_w <- french_word  %>% filter(corpus_name=="Geneva")
french_w <- french_w[order(french_w$transcript), ]  %>% filter(!incomplete_type =="omission") #   %>%   ###remove extra sessions

#fren <- french_w[french_w$gloss!=york$gloss_childes, ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

geneva <- get_tokens(corpus="Geneva", token="*")

geneva <- geneva %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
geneva <- geneva[order(geneva$filename), ]
geneva <- tibble::rowid_to_column(geneva, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Geneva")

#french_w$gloss_ann <- ifelse(french_w$gloss > york$gloss_childes, 'DIFFERENT', ifelse(french_w$transcript == york$filename, '', 'DIFFERENT'))  ###check matching

geneva_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(geneva)

final_geneva <- french %>% left_join(geneva_childes)

#################################################

transcript <- get_transcripts(corpus="Hammelrath")

french_w <- french_word  %>% filter(corpus_name=="Hammelrath")
french_w <- french_w[order(french_w$transcript), ]  %>% filter(!incomplete_type =="omission") #   %>%   ###remove extra sessions

#fren <- french_w[french_w$gloss!=york$gloss_childes, ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

hammelrath <- get_tokens(corpus="Hammelrath", token="*")

hammelrath <- hammelrath %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
hammelrath <- hammelrath[order(hammelrath$filename), ]
hammelrath <- tibble::rowid_to_column(hammelrath, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Hammelrath")

#french_w$gloss_ann <- ifelse(french_w$gloss > york$gloss_childes, 'DIFFERENT', ifelse(french_w$transcript == york$filename, '', 'DIFFERENT'))  ###check matching

hammelrath_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(hammelrath)

final_hammelrath <- french %>% left_join(hammelrath_childes)


#################################

transcript <- get_transcripts(corpus="Leveille")

french_w <- french_word  %>% filter(corpus_name=="Leveille")
french_w <- french_w[order(french_w$transcript), ]  %>% filter(!incomplete_type =="omission") #   %>%   ###remove extra sessions

#fren <- french_w[french_w$gloss!=york$gloss_childes, ]

french_w <- french_w[-c(66189), ]
french_w <- french_w[-c(103372), ]

french_w <- tibble::rowid_to_column(french_w, "matching_index")

leveille <- get_tokens(corpus="Leveille", token="*")

leveille <- leveille %>% left_join(transcript) %>%
  mutate(filename = gsub("French/", "", filename))%>%
  rename(gloss_childes=gloss)
leveille <- leveille[order(leveille$filename), ]
leveille <- tibble::rowid_to_column(leveille, "matching_index")

french <- french_base%>%
  filter(corpus_name=="Leveille")

#french_w$gloss_ann <- ifelse(french_w$gloss > leveille$gloss_childes, 'DIFFERENT', ifelse(french_w$transcript == leveille$filename, '', 'DIFFERENT'))  ###check matching

leveille_childes <- french_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(leveille)

final_leveille <- french %>% left_join(leveille_childes)

###########################################

french_morphology <- do.call("rbind", list(final_champaud, final_palasis, final_pauline, final_york, final_geneva, final_hammelrath, final_leveille)) %>%
  select(id, pos, stem, affix, affix_type, gloss, corpus_name, prefix, suffix, utterance_id) %>%
  group_by(id, utterance_id, corpus_name, gloss) %>%
  summarise(stem = paste(sort(unique(stem)),collapse=", "),
            affix = paste(sort(unique(affix)),collapse=", "),
            affix_type = paste(sort(unique(affix_type)),collapse=", "),
            gloss = paste(sort(unique(gloss)),collapse=", "),
            prefix = paste(sort(unique(prefix)),collapse=", "),
            suffix = paste(sort(unique(suffix)),collapse=", "),
            pos = paste(sort(unique(pos)),collapse=", ")) %>%
  rename(stem_m=stem) %>% rename(affix_m=affix) %>%
  rename(affix_type_m=affix_type) %>% rename(gloss_m=gloss) %>%
  rename(prefix_m=prefix) %>%  rename(suffix_m=suffix)


tokens_fra <- get_tokens(language = "fra", token="*", role_exclude = "Target_Child")
tokens_fra_ <- tokens_fra %>%
  left_join(french_morphology) %>%
  filter(!is.na(gloss_m)) %>%
  rename(token_id=id) %>%
  mutate(affix_m = lapply(strsplit(as.character(affix_m),split=','),trimws))




saveRDS(tokens_fra_, "~/Documents/morphological_predictors/french_morph.rds")

