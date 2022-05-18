library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)

############################# US

mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/mor.rds')
mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/mk.rds')
word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-NA/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-NA/", "", transcript)) %>% #Eng-NA
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "eng", token="*") #role_exclude = "Target_Child"

base <- mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-NA/", "", transcript)) %>%
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
  new_morphology__ <- new_morphology_ %>%
    rename(stem_m=stem) %>% rename(affix_m=affix) %>%
    rename(affix_type_m=affix_type) %>% rename(gloss_m=gloss) %>%
    rename(prefix_m=prefix) %>%  rename(suffix_m=suffix)
  print(new_morphology__)
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
######################################## Garvey

final_garvey <- match_morph_to_childes("Garvey", "Eng-NA/")
garvey_morphology <- summ_morph(final_garvey)
################

final_valian<- match_morph_to_childes("Valian", "Eng-NA/")
valian_morphology <- summ_morph(final_valian)
################

final_bernstein <- match_morph_to_childes("Bernstein","Eng-NA/")
bernstein_morphology <- summ_morph(final_bernstein)
################

final_petersonmccabe <- match_morph_to_childes("PetersonMcCabe", "Eng-NA/")
petersonmccabe_morphology <- summ_morph(final_petersonmccabe)
################

final_hall <- match_morph_to_childes("Hall", "Eng-NA/")
hall_morphology <- summ_morph(final_hall)
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
peters_morphology <- summ_morph(final_peters)
###################

final_bliss <- match_morph_to_childes("Bliss", "Eng-NA/")
bliss_morphology <- summ_morph(final_bliss)
###################

final_brent <- match_morph_to_childes("Brent", "Eng-NA/")
brent_morphology <- summ_morph(final_brent)

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
brown_morphology <- summ_morph(final_brown)
##################### Gopnik

transcript <- get_transcripts(corpus="Gopnik")

w <- word  %>% filter(corpus_name=="Gopnik")
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
gopnik_morphology <- summ_morph(final_gopnik)
############## Gleason

final_gleason <- match_morph_to_childes("Gleason", "Eng-NA/")
gleason_morphology <- summ_morph(final_gleason)
####################

final_haggerty <- match_morph_to_childes("Haggerty", "Eng-NA/")
haggerty_morphology <- summ_morph(final_haggerty)
###################

final_hicks <- match_morph_to_childes( "Hicks", "Eng-NA/")
hicks_morphology <- summ_morph(final_hicks)
##################

final_higginson <- match_morph_to_childes("Higginson", "Eng-NA/")
higginson_morphology <- summ_morph(final_higginson)
##################

final_kuczaj <- match_morph_to_childes("Kuczaj", "Eng-NA/")
kuczaj_morphology <- summ_morph(final_kuczaj)
##################

final_mccune <- match_morph_to_childes( "McCune", "Eng-NA/")
mccune_morphology <- summ_morph(final_mccune)
##################

final_mcmillan <- match_morph_to_childes("McMillan", "Eng-NA/")
mcmillan_morphology <- summ_morph(final_mcmillan)
##################

final_morisset <- match_morph_to_childes("Morisset", "Eng-NA/")
morisset_morphology <- summ_morph(final_morisset)
##################

final_nelson <- match_morph_to_childes( "Nelson", "Eng-NA/")
nelson_morphology <- summ_morph(final_nelson)
##################

final_newengland <- match_morph_to_childes("NewEngland", "Eng-NA/")
newengland_morphology <- summ_morph(final_newengland)
##################

final_post <- match_morph_to_childes( "Post", "Eng-NA/")
post_morphology <- summ_morph(final_post)
##################

final_rollins <- match_morph_to_childes( "Rollins", "Eng-NA/")
rollins_morphology <- summ_morph(final_rollins)
##################

final_sachs <- match_morph_to_childes("Sachs", "Eng-NA/")
sachs_morphology <- summ_morph(final_sachs)
##################

final_sawyer <- match_morph_to_childes( "Sawyer","Eng-NA/")
sawyer_morphology <- summ_morph(final_sawyer)
##################

final_snow <- match_morph_to_childes("Snow", "Eng-NA/")
snow_morphology <- summ_morph(final_snow)
##################

final_soderstrom <- match_morph_to_childes( "Soderstrom", "Eng-NA/")
soderstrom_morphology <- summ_morph(final_soderstrom)
##################

final_sprott <- match_morph_to_childes( "Sprott", "Eng-NA/")
sprott_morphology <- summ_morph(final_sprott)
##################

final_suppes <- match_morph_to_childes("Suppes", "Eng-NA/")
suppes_morphology <- summ_morph(final_suppes)
##################

final_tardif <- match_morph_to_childes( "Tardif", "Eng-NA/")
tardif_morphology <- summ_morph(final_tardif)
##################

final_vanhouten <- match_morph_to_childes( "VanHouten", "Eng-NA/")
vanhouten_morphology <- summ_morph(final_vanhouten)
##################

final_vankleeck <- match_morph_to_childes( "VanKleeck", "Eng-NA/")
vankleeck_morphology <- summ_morph(final_vankleeck)
##################

final_warren <- match_morph_to_childes( "Warren", "Eng-NA/")

##################

final_weist <- match_morph_to_childes( "Weist", "Eng-NA/")
weist_morphology <- summ_morph(final_weist)
##################

us_morphology <- do.call("rbind", list(weist_morphology, vankleeck_morphology, vanhouten_morphology, tardif_morphology, suppes_morphology, sprott_morphology, soderstrom_morphology, snow_morphology, sawyer_morphology, sachs_morphology, rollins_morphology, post_morphology, newengland_morphology, nelson_morphology, morisset_morphology, mcmillan_morphology, mccune_morphology, kuczaj_morphology, higginson_morphology, hicks_morphology, haggerty_morphology, gleason_morphology, gopnik_morphology, brown_morphology, brent_morphology, bliss_morphology, peters_morphology, hall_morphology, petersonmccabe_morphology, bernstein_morphology, valian_morphology, garvey_morphology ))
saveRDS(us_morphology, "/Users/loukatou/Desktop/morph_eng.rds")


tokens_us <- get_tokens(collection = "Eng-NA", token="*", role_exclude = "Target_Child")
tokens_us_ <- tokens_us %>%
  left_join(us_morphology)

tokens_us__ <- tokens_us_ %>%
  filter(!is.na(gloss_m)) %>%
  rename(token_id=id) %>%
  mutate(affix_m = lapply(strsplit(as.character(affix_m),split=','),trimws))


saveRDS(tokens_us__, "/Users/loukatou/Documents/aoa-pipeline/data/childes/morph_eng.rds")




#######################################################################################
####################################################################################### UK

mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/mor.rds')
mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/mk.rds')
word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-UK/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "eng", token="*") #role_exclude = "Target_Child"

base <- mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-UK/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

########################################

final_belfast <- match_morph_to_childes("Belfast", "Eng-UK/")
belfast_morphology <- summ_morph(final_belfast)
########################################

final_fletcher <- match_morph_to_childes("Fletcher", "Eng-UK/")
fletcher_morphology <- summ_morph(final_fletcher)

########################################

final_gathburn <- match_morph_to_childes("Gathburn", "Eng-UK/")
gathburn_morphology <- summ_morph(final_gathburn)

########################################

final_howe<-match_morph_to_childes("Howe", "Eng-UK/")
howe_morphology <- summ_morph(final_howe)

########################################

final_korman<-match_morph_to_childes("Korman", "Eng-UK/")
korman_morphology <- summ_morph(final_korman)

########################################

final_lara<-match_morph_to_childes("Lara", "Eng-UK/")
lara_morphology <- summ_morph(final_lara)

########################################

final_manchester<-match_morph_to_childes("Manchester", "Eng-UK/")
manchester_morphology <- summ_morph(final_manchester)

########################################

final_mpi_eva_manchester<-match_morph_to_childes("MPI-EVA-Manchester", "Eng-UK/")
mpi_eva_manchester_morphology <- summ_morph(final_mpi_eva_manchester)

########################################

final_nuffield<-match_morph_to_childes("Nuffield", "Eng-UK/")
nuffield_morphology <- summ_morph(final_nuffield)

########################################

final_thomas<-match_morph_to_childes("Thomas", "Eng-UK/")
thomas_morphology <- summ_morph(final_thomas)

########################################

final_tommerdahl<-match_morph_to_childes("Tommerdahl", "Eng-UK/")
tommerdahl_morphology <- summ_morph(final_tommerdahl)

########################################

final_wells<-match_morph_to_childes( "Wells", "Eng-UK/")
wells_morphology <- summ_morph(final_wells)

########################################

uk_morphology <- do.call("rbind", list(wells_morphology, howe_morphology, thomas_morphology, belfast_morphology, fletcher_morphology, gathburn_morphology, howe_morphology, mpi_eva_manchester_morphology, nuffield_morphology,  lara_morphology, manchester_morphology, tommerdahl_morphology, korman_morphology))
saveRDS(uk_morphology, "/Users/loukatou/Desktop/morph_eng_uk.rds")

tokens_uk <- get_tokens(collection = "Eng-UK", token="*", role_exclude = "Target_Child")
tokens_uk_ <- tokens_uk %>%
  left_join(uk_morphology)

tokens_uk__ <- tokens_uk_ %>%
  filter(!is.na(gloss_m)) %>%
  rename(token_id=id) %>%
  mutate(affix_m = lapply(strsplit(as.character(affix_m),split=','),trimws))

saveRDS(tokens_uk__, "/Users/loukatou/Documents/aoa-pipeline/data/childes/morph_eng_uk.rds")

##################################################################################
################################################################################## Spa

mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/mor.rds')
mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/mk.rds')
word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Spanish/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "spa", token="*")

base <- mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Spanish/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

########################################

final_aguirre<-match_morph_to_childes( "Aguirre", "Spanish/")
aguirre_morphology <- summ_morph(final_aguirre)

########################################

transcript <- get_transcripts(corpus="FernAguado")

w <- word  %>% filter(corpus_name=="FernAguado")
w <- w[order(w$transcript), ]

corpus <- childes %>% filter(corpus_name=="FernAguado") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)
w <- w[-c(280696), ]

w <- tibble::rowid_to_column(w, "matching_index")
corpus <- tibble::rowid_to_column(corpus, "matching_index")


#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="FernAguado") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_fernaguado<- language %>% left_join(corpus_childes)
fernaguado_morphology <- summ_morph(final_fernaguado)

########################################

final_marrero <-match_morph_to_childes( "Marrero", "Spanish/")
marrero_morphology <- summ_morph(final_marrero)

########################################

final_grerli<-match_morph_to_childes( "GRERLI", "Spanish/")
grerli_morphology <- summ_morph(final_grerli)

########################################

final_hess<-match_morph_to_childes( "Hess", "Spanish/")
hess_morphology <- summ_morph(final_hess)

########################################

final_colmex <-match_morph_to_childes( "ColMex", "Spanish/")
colmex_morphology <- summ_morph(final_colmex)

########################################

transcript <- get_transcripts(corpus="Remedi")

w <- word  %>% filter(corpus_name=="Remedi")
w <- w[order(w$transcript), ]

corpus <- childes %>% filter(corpus_name=="Remedi") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)
w <- w[-c(7077:7080), ]

w <- tibble::rowid_to_column(w, "matching_index")
corpus <- tibble::rowid_to_column(corpus, "matching_index")


#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Remedi") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_remedi<- language %>% left_join(corpus_childes)
remedi_morphology <- summ_morph(final_remedi)

########################################

final_serrasole <-match_morph_to_childes( "SerraSole", "Spanish/")
serrasole_morphology <- summ_morph(final_serrasole)

########################################

spa_morphology <- do.call("rbind", list(serrasole_morphology, remedi_morphology, colmex_morphology, hess_morphology, grerli_morphology, marrero_morphology, fernaguado_morphology, aguirre_morphology))

tokens_spa <- get_tokens(collection = "Spanish", token="*", role_exclude = "Target_Child")
tokens_spa_ <- tokens_spa %>%
  left_join(spa_morphology)

tokens_spa__ <- tokens_spa_ %>%
  filter(!is.na(gloss_m)) %>%
  rename(token_id=id) %>%
  mutate(affix_m = lapply(strsplit(as.character(affix_m),split=','),trimws))

saveRDS(tokens_spa__, "/Users/loukatou/Documents/aoa-pipeline/data/childes/morph_spa.rds")






