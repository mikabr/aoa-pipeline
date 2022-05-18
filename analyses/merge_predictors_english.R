library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)

############################# US

english_us_mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/mor.rds')
english_us_mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/mk.rds')
english_us_word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-NA/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "eng", token="*") #role_exclude = "Target_Child"

base <- english_us_mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(english_us_mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-NA/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)


########################################

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
  new_morphology__ <- new_morphology_ %>%
    rename(stem_m=stem) %>% rename(affix_m=affix) %>%
    rename(affix_type_m=affix_type) %>% rename(gloss_m=gloss) %>%
    rename(prefix_m=prefix) %>%  rename(suffix_m=suffix)
  print(new_morphology__)
}


match_morph_to_childes <- function(name_collection, name_corpus){
  transcript <- get_transcripts(corpus=name_corpus)

  w <- english_us_word  %>% filter(corpus_name==name_corpus)
  w <- w[order(w$transcript), ]   ###remove extra sessions

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
  language <- base %>%  filter(corpus_name==name_corpus) %>%   ###remove extra sessions
    filter(transcript %in% a)

  corpus <- tibble::rowid_to_column(corpus, "matching_index")
  w <- tibble::rowid_to_column(w, "matching_index")

  corpus_childes <- w %>%
    rename(morph_id=id ) %>%
    select(-corpus_name ) %>%
    distinct() %>%
    left_join(corpus)

  final <- language %>% left_join(corpus_childes)
  print(final)
}
######################################## Garvey


transcript <- get_transcripts(corpus="Garvey")

w <- english_us_word  %>% filter(corpus_name=="Garvey")
w <- w[order(w$transcript), ]


corpus <- childes %>% filter(corpus_name=="Garvey") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

corpus <- tibble::rowid_to_column(corpus, "matching_index")
w <- tibble::rowid_to_column(w, "matching_index")
#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Garvey") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_garvey <- language %>% left_join(corpus_childes)

################

final_valian<- match_morph_to_childes("Eng-NA/", "Valian")

################

final_bernstein <- match_morph_to_childes("Eng-NA/", "Bernstein")

################

final_PetersonMcCabe <- match_morph_to_childes("Eng-NA/", "PetersonMcCabe")

################

transcript <- get_transcripts(corpus="Hall")

w <- english_us_word  %>% filter(corpus_name=="Hall")
w <- w[order(w$transcript), ] %>%
  filter(transcript != "Hall/BlackPro/anc.xml" )%>%
  filter(transcript != "Hall/WhiteWork/kao.xml" )

corpus <- childes %>% filter(corpus_name=="Hall") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ] %>%
  filter(filename != "Hall/BlackPro/anc.xml" ) %>%
  filter(filename != "Hall/WhiteWork/kao.xml" )

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

corpus <- tibble::rowid_to_column(corpus, "matching_index")
w <- tibble::rowid_to_column(w, "matching_index")
#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Hall") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_hall<- language %>% left_join(corpus_childes)

###################

transcript <- get_transcripts(corpus="Peters")


w <- english_us_word  %>% filter(corpus_name=="Peters")
w <- w[order(w$transcript), ]  %>%
  filter(transcript != "Peters/010410a.xml" ) %>%
  filter(transcript != "Peters/010410b.xml" ) %>%
  filter(transcript != "Peters/010502a.xml" ) %>%
  filter(transcript != "Peters/010524.xml" ) %>%
  filter(transcript != "Peters/010528.xml" ) %>%
  filter(transcript != "Peters/010600.xml" ) %>%
  filter(transcript != "Peters/010607b.xml" ) %>%
  filter(transcript != "Peters/010614a.xml" ) %>%
  filter(transcript != "Peters/010614b.xml" ) %>%
  filter(transcript != "Peters/010621a.xml" )%>%
  filter(transcript != "Peters/010621b.xml" ) %>%
  filter(transcript != "Peters/010626.xml" ) %>%
  filter(transcript != "Peters/010627.xml" ) %>%
  filter(transcript != "Peters/010810.xml" ) %>%
  filter(transcript != "Peters/010816.xml" )  %>%
  filter(transcript != "Peters/010817.xml" ) %>%
  filter(transcript != "Peters/010822a.xml" ) %>%
  filter(transcript != "Peters/010822b.xml" ) %>%
  filter(transcript != "Peters/010901a.xml" ) %>%
  filter(transcript != "Peters/010901b.xml" )

corpus <- childes %>% filter(corpus_name=="Peters") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ] %>%
  filter(filename != "Peters/010410a.xml" ) %>%
  filter(filename != "Peters/010410b.xml" ) %>%
  filter(filename != "Peters/010502a.xml" ) %>%
  filter(filename != "Peters/010524.xml" ) %>%
  filter(filename != "Peters/010528.xml" ) %>%
  filter(filename != "Peters/010600.xml" ) %>%
  filter(filename != "Peters/010607b.xml" ) %>%
  filter(filename != "Peters/010614a.xml" )%>%
  filter(filename != "Peters/010614b.xml" ) %>%
  filter(filename != "Peters/010621a.xml" ) %>%
  filter(filename != "Peters/010621b.xml" )%>%
  filter(filename != "Peters/010626.xml" )%>%
  filter(filename != "Peters/010627.xml" )%>%
  filter(filename != "Peters/010810.xml" ) %>%
  filter(filename != "Peters/010816.xml" ) %>%
  filter(filename != "Peters/010817.xml" ) %>%
  filter(filename != "Peters/010822a.xml" ) %>%
  filter(filename != "Peters/010822b.xml" ) %>%
  filter(filename != "Peters/010901a.xml" ) %>%
  filter(filename != "Peters/010901b.xml" )

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

corpus <- tibble::rowid_to_column(corpus, "matching_index")
w <- tibble::rowid_to_column(w, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

corpus <-corpus[0:30025,]
w <-w[0:30025,]

language <- base %>%  filter(corpus_name=="Peters") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_peters<- language %>% left_join(corpus_childes)
final_peters <-final_peters[0:30006,]

###################

#Bliss

transcript <- get_transcripts(corpus="Bliss")

a<-intersect(unique(w$transcript), unique(corpus$filename))

w <- english_us_word  %>% filter(corpus_name=="Bliss")
w <- w[order(w$transcript), ]%>%
  filter(transcript != "Clinical-MOR/Bliss/impsarah.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impjoel.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impfred.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impterra.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impjohn.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impjim.xml" ) %>%
  filter(transcript != "Clinical-MOR/Bliss/impdenis.xml" )


corpus <- childes %>% filter(corpus_name=="Bliss") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss) %>%
  filter(filename != "Clinical-MOR/Bliss/impsarah.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impjoel.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impfred.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impterra.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impjohn.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impjim.xml" ) %>%
  filter(filename != "Clinical-MOR/Bliss/impdenis.xml" )
corpus <- corpus[order(corpus$filename), ]


a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

w <- tibble::rowid_to_column(w, "matching_index")
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Bliss") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_bliss<- language %>% left_join(corpus_childes)

###################

transcript <- get_transcripts(corpus="Brent")

w <- english_us_word  %>% filter(corpus_name=="Brent")
w <- w[order(w$transcript), ]

corpus <- childes %>% filter(corpus_name=="Brent") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

w <- tibble::rowid_to_column(w, "matching_index")
corpus <- tibble::rowid_to_column(corpus, "matching_index")

language <- base %>%  filter(corpus_name=="Brent") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_brent <- language %>% left_join(corpus_childes)

#########################

transcript <- get_transcripts(corpus="Brown")

w <- english_us_word  %>% filter(corpus_name=="Brown")
w <- w[order(w$transcript), ] %>%
  filter(transcript != "Brown/Adam/020304.xml" ) %>%
  filter(transcript != "Brown/Adam/020403.xml" ) %>%
  filter(transcript != "Brown/Adam/020415.xml")

corpus <- childes %>% filter(corpus_name=="Brown") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss) %>%
  filter(filename != "Brown/Adam/020304.xml" ) %>%
  filter(filename != "Brown/Adam/020403.xml") %>%
  filter(filename != "Brown/Adam/020415.xml")
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

corpus <- tibble::rowid_to_column(corpus, "matching_index")
w <- tibble::rowid_to_column(w, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Brown") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_brown <- language %>% left_join(corpus_childes)

##################### Gopnik

transcript <- get_transcripts(corpus="Gopnik")

w <- english_us_word  %>% filter(corpus_name=="Gopnik")
w <- w[order(w$transcript), ]

corpus <- childes %>% filter(corpus_name=="Gopnik") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

w <- tibble::rowid_to_column(w, "matching_index")
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Gopnik") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_gopnik<- language %>% left_join(corpus_childes)

############## Gleason
transcript <- get_transcripts(corpus="Gleason")

w <- english_us_word  %>% filter(corpus_name=="Gleason")
w <- w[order(w$transcript), ]

corpus <- childes %>% filter(corpus_name=="Gleason") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-NA/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)

corpus <- tibble::rowid_to_column(corpus, "matching_index")
w <- tibble::rowid_to_column(w, "matching_index")
#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Gleason") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_gleason<- language %>% left_join(corpus_childes)

####################

final_haggerty <- match_morph_to_childes("Eng-NA/", "Haggerty")

###################

final_hicks <- match_morph_to_childes("Eng-NA/", "Hicks")

##################

final_higginson <- match_morph_to_childes("Eng-NA/", "Higginson")

##################

final_kuczaj <- match_morph_to_childes("Eng-NA/", "Kuczaj")

##################

final_mccune <- match_morph_to_childes("Eng-NA/", "McCune")

##################

final_mcmillan <- match_morph_to_childes("Eng-NA/", "McMillan")

##################

final_morisset <- match_morph_to_childes("Eng-NA/", "Morisset")

##################

final_nelson <- match_morph_to_childes("Eng-NA/", "Nelson")

##################

final_newengland <- match_morph_to_childes("Eng-NA/", "NewEngland")

##################

final_post <- match_morph_to_childes("Eng-NA/", "Post")

##################

final_rollins <- match_morph_to_childes("Eng-NA/", "Rollins")

##################

final_sachs <- match_morph_to_childes("Eng-NA/", "Sachs")

##################

final_sawyer <- match_morph_to_childes("Eng-NA/", "Sawyer")

##################

final_snow <- match_morph_to_childes("Eng-NA/", "Snow")

##################

final_soderstrom <- match_morph_to_childes("Eng-NA/", "Soderstrom")

##################

final_sprott <- match_morph_to_childes("Eng-NA/", "Sprott")

##################

final_suppes <- match_morph_to_childes("Eng-NA/", "Suppes")

##################

final_tardif <- match_morph_to_childes("Eng-NA/", "Tardif")

##################

final_vanhouten <- match_morph_to_childes("Eng-NA/", "VanHouten")

##################

final_vankleeck <- match_morph_to_childes("Eng-NA/", "VanKleeck")

##################

final_warren <- match_morph_to_childes("Eng-NA/", "Warren")

##################

final_weist <- match_morph_to_childes("Eng-NA/", "Weist")

##################


weist_morphology <- summ_morph(final_weist)

vankleeck_morphology <- summ_morph(final_vankleeck)

vanhouten_morphology <- summ_morph(final_vanhouten)

tardif_morphology <- summ_morph(final_tardif)

#suppes_morphology <- summ_morph(final_suppes)

sprott_morphology <- summ_morph(final_sprott)

soderstrom_morphology <- summ_morph(final_soderstrom)

snow_morphology <- summ_morph(final_snow)

sawyer_morphology <- summ_morph(final_sawyer)

sachs_morphology <- summ_morph(final_sachs)

rollins_morphology <- summ_morph(final_rollins)

post_morphology <- summ_morph(final_post)

newengland_morphology <- summ_morph(final_newengland)

nelson_morphology <- summ_morph(final_nelson)

morisset_morphology <- summ_morph(final_morisset)

mcmillan_morphology <- summ_morph(final_mcmillan)

mccune_morphology <- summ_morph(final_mccune)

kuczaj_morphology <- summ_morph(final_kuczaj)

higginson_morphology <- summ_morph(final_higginson)

hicks_morphology <- summ_morph(final_hicks)

haggerty_morphology <- summ_morph(final_haggerty)

gleason_morphology <- summ_morph(final_gleason)

gopnik_morphology <- summ_morph(final_gopnik)

brown_morphology <- summ_morph(final_brown)

brent_morphology <- summ_morph(final_brent)

bliss_morphology <- summ_morph(final_bliss)

peters_morphology <- summ_morph(final_peters)

hall_morphology <- summ_morph(final_hall)

PetersonMcCabe_morphology <- summ_morph(final_PetersonMcCabe)

bernstein_morphology <- summ_morph(final_bernstein)

valian_morphology <- summ_morph(final_valian)

garvey_morphology <- summ_morph(final_garvey)


us_morphology <- do.call("rbind", list(weist_morphology, vankleeck_morphology, vanhouten_morphology, tardif_morphology, suppes_morphology, sprott_morphology, soderstrom_morphology, snow_morphology, sawyer_morphology, sachs_morphology, rollins_morphology, post_morphology, newengland_morphology, nelson_morphology, morisset_morphology, mcmillan_morphology, mccune_morphology, kuczaj_morphology, higginson_morphology, hicks_morphology, haggerty_morphology, gleason_morphology, gopnik_morphology, brown_morphology, brent_morphology, bliss_morphology, peters_morphology, PetersonMcCabe_morphology, bernstein_morphology, valian_morphology, garvey_morphology ))


tokens_us <- get_tokens(collection = "Eng-NA", token="*", role_exclude = "Target_Child")
tokens_us_ <- tokens_us %>%
  left_join(us_morphology)

saveRDS(tokens_us_, "~/Documents/morphological_predictors/us_morph.rds")






