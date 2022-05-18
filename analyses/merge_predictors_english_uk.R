library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)


english_uk_mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/mor.rds')
english_uk_mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/mk.rds')
english_uk_word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Eng-UK/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-UK/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "eng", token="*") #role_exclude = "Target_Child"

base <- english_uk_mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(english_uk_mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Eng-UK/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

########################################

##Belfast

transcript <- get_transcripts(corpus="Belfast")

w <- english_uk_word  %>% filter(corpus_name=="Belfast")
w <- w[order(w$transcript), ] #%>%   ###remove extra sessions
#filter(transcript != "Champaud/011003.xml" )
w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="Belfast") %>% left_join(transcript) %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > aguirre$gloss_childes, 'DIFFERENT',
#                      ifelse(spanish_w$transcript == aguirre$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%
  filter(corpus_name=="Belfast")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_belfast<- language %>% left_join(corpus_childes)

##Fletcher

transcript <- get_transcripts(corpus="Fletcher")

w <- english_uk_word  %>% filter(corpus_name=="Fletcher")
w <- w[order(w$transcript), ] #%>%   ###remove extra sessions
#filter(transcript != "Champaud/011003.xml" )
w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="Fletcher") %>% left_join(transcript) %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > aguirre$gloss_childes, 'DIFFERENT',
#                      ifelse(spanish_w$transcript == aguirre$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Fletcher")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_fletcher<- language %>% left_join(corpus_childes)

###### Gathburn

transcript <- get_transcripts(corpus="Gathburn")

w <- english_uk_word  %>% filter(corpus_name=="Gathburn")
w <- w[order(w$transcript), ] #%>%   ###remove extra sessions
#filter(transcript != "Champaud/011003.xml" )
w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="Gathburn") %>% left_join(transcript) %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > aguirre$gloss_childes, 'DIFFERENT',
#                      ifelse(spanish_w$transcript == aguirre$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Gathburn")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_gathburn<- language %>% left_join(corpus_childes)

####### Howe

final_howe<-match_morph_to_childes("Eng-UK/", "Howe")

##################def


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

############ Korman

final_korman<-match_morph_to_childes("Eng-UK/", "Korman")

############ Lara

transcript <- get_transcripts(corpus="Lara")

w <- english_uk_word  %>% filter(corpus_name=="Lara")
w <- w[order(w$transcript), ] #%>%   ###remove extra sessions
w <-w[0:515725,]

w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="Lara") %>% left_join(transcript) %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]#%>%   ###remove extra sessions
corpus <-corpus[0:515725,]

corpus <- tibble::rowid_to_column(corpus, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching


language <- base %>%  filter(corpus_name=="Lara")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_lara <- language %>% left_join(corpus_childes)

################# Manchester
transcript <- get_transcripts(corpus="Manchester")

w <- english_uk_word  %>% filter(corpus_name=="Manchester")
w <- w[order(w$transcript), ] #%>%   ###remove extra sessions

w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="Manchester") %>% left_join(transcript) %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]#%>%   ###remove extra sessions
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching


language <- base %>%  filter(corpus_name=="Manchester")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_manchester <- language %>% left_join(corpus_childes)


################# MPI-EVA-Manchester TO REDO

transcript <- get_transcripts(corpus="MPI-EVA-Manchester")


w <- english_uk_word  %>% filter(corpus_name=="MPI-EVA-Manchester")
w <- w[order(w$transcript), ]
w <- tibble::rowid_to_column(w, "matching_index")

corpus <- childes %>% filter(corpus_name=="MPI-EVA-Manchester") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]
#corpus <-corpus[0:1823831,]
corpus <- tibble::rowid_to_column(corpus, "matching_index")

a<-intersect(unique(w$transcript), unique(corpus$filename))

corpus <- corpus %>%
  filter(filename %in% a)
w <- w  %>%
  filter(transcript %in% a)
#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="MPI-EVA-Manchester") %>%   ###remove extra sessions
  filter(transcript %in% a)

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_mpi_eva_manchester <- language %>% left_join(corpus_childes)


####### Nuffield

transcript <- get_transcripts(corpus="Nuffield")

w <- english_uk_word  %>% filter(corpus_name=="Nuffield")
w <- w[order(w$transcript), ] %>%   ###remove extra sessions
  filter(transcript != "Nuffield/089.xml" )
w <- tibble::rowid_to_column(w, "matching_index")


corpus <- childes %>% filter(corpus_name=="Nuffield") %>% left_join(transcript)
corpus <- corpus %>%
  mutate(filename = gsub("Eng-UK/", "", filename)) %>%
  rename(gloss_childes=gloss)
corpus <- corpus[order(corpus$filename), ]%>%   ###remove extra sessions
  filter(filename != "Nuffield/089.xml" )
corpus <- tibble::rowid_to_column(corpus, "matching_index")

#w$gloss_ann <-ifelse(w$gloss > corpus$gloss_childes, 'DIFFERENT',
#                      ifelse(w$transcript == corpus$filename, '', 'DIFFERENT'))  ###check matching

language <- base %>%  filter(corpus_name=="Nuffield")

corpus_childes <- w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(corpus)

final_nuffield <- language %>% left_join(corpus_childes)

####### Thomas

final_thomas<-match_morph_to_childes("Eng-UK/", "Thomas")

######Tommerdahl

final_tommerdahl<-match_morph_to_childes("Eng-UK/", "Tommerdahl")

###### Wells

final_wells<-match_morph_to_childes("Eng-UK/", "Wells")

######


# final_thomas,  final_belfast, final_fletcher, final_gathburn, final_howe, final_mpi_eva_manchester, final_nuffield,  final_lara, final_manchester, , final_tommerdahl, final_korman
howe_morphology <- do.call("rbind", list( final_howe)) %>%
  select(id, stem, pos, prefix, affix, affix_type, gloss, corpus_name, suffix, utterance_id)

#uk_morphology_ex <- uk_morphology[-c(3573492), ]
#uk_morphology_1 <- uk_morphology_ex[0:4566234, ]
#uk_morphology_2 <- uk_morphology_ex[4566235:9132468, ]
#uk_morphology_1 <- uk_morphology[0:1000000, ]


howe_morphology <- howe_morphology %>%
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


uk_morphology <- do.call("rbind", list(lara_morphology,howe_morphology,tommerdahl_morphology,manchester_morphology,wells_morphology,thomas_morphology,belfast_morphology,fletcher_morphology,gathburn_morphology,mpi_eva_manchester_morphology,nuffield_morphology))


tokens_uk <- get_tokens(collection = "Eng-UK", token="*", role_exclude = "Target_Child")
tokens_uk_ <- tokens_uk %>%
  left_join(uk_morphology)

saveRDS(tokens_uk_, "~/Documents/morphological_predictors/uk_morph.rds")


