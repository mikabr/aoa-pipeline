library(tidyverse)
library(childesr)
library(dplyr)
library(Hmisc)
library(gdata)


spanish_mor <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/mor.rds')
spanish_mk <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/mk.rds')
spanish_word <- readRDS('/Users/loukatou/Documents/morphological_predictors/childes-mor-data/childes/Spanish/w.rds') %>%
  dplyr::select(-pos) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Spanish/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript)) %>%
  mutate(incomplete_type=ifelse(is.na(incomplete_type), "0", incomplete_type))%>%
  filter(!incomplete_type =="omission")

childes <- get_tokens(language = "spa", token="*") #role_exclude = "Target_Child"

spanish_base <- spanish_mor %>%
  rename(mor_fk=id) %>%
  dplyr::select(-file) %>%
  left_join(spanish_mk) %>%
  dplyr::select(-file) %>%
  rename(mk_fk=id) %>%
  mutate(transcript = gsub("/Users/mikabr/childes/childes/Spanish/", "", transcript)) %>%
  mutate(corpus_name = gsub("/.*", "", transcript))%>%
  rename(stem_right = stem) %>%
  rename(english_old = english) %>%
  rename(morph_id = w_fk)

########################################

##Aguirre

transcript <- get_transcripts(corpus="Aguirre")

spanish_w <- spanish_word  %>% filter(corpus_name=="Aguirre")
spanish_w <- spanish_w[order(spanish_w$transcript), ] #%>%   ###remove extra sessions
  #filter(transcript != "Champaud/011003.xml" )
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

aguirre <- childes %>% filter(corpus_name=="Aguirre") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
aguirre <- aguirre[order(aguirre$filename), ]
aguirre <- tibble::rowid_to_column(aguirre, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > aguirre$gloss_childes, 'DIFFERENT',
#                      ifelse(spanish_w$transcript == aguirre$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="Aguirre")

aguirre_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(aguirre)

final_aguirre<- spanish %>% left_join(aguirre_childes)

################################

#FernAguado

transcript <- get_transcripts(corpus="FernAguado")

spanish_w <- spanish_word  %>% filter(corpus_name=="FernAguado")
spanish_w <- spanish_w[order(spanish_w$transcript), ] %>%   ###remove extra sessions
  filter(transcript != "FernAguado/Eider/030603a.xml" )
spanish_w <- spanish_w[-c(280696), ]
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")


fernaguado <- childes %>% filter(corpus_name=="FernAguado") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
fernaguado <- fernaguado[order(fernaguado$filename), ]
fernaguado <- tibble::rowid_to_column(fernaguado, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > fernaguado$gloss_childes, 'DIFFERENT',
#                      ifelse(spanish_w$transcript == fernaguado$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="FernAguado")

fernaguado_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(fernaguado)

final_fernaguado<- spanish %>% left_join(fernaguado_childes)


################################

#Marrero

transcript <- get_transcripts(corpus="Marrero")

spanish_w <- spanish_word  %>% filter(corpus_name=="Marrero")
spanish_w <- spanish_w[order(spanish_w$transcript), ] %>%   ###remove extra sessions
  filter(transcript != "Marrero/Rafael/040626.xml" ) %>%
  filter(transcript != "Marrero/Rafael/040416.xml" ) %>%
  filter(transcript != "Marrero/Idaira/030710.xml" )


spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")


marrero <- childes %>% filter(corpus_name=="Marrero") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
marrero <- marrero[order(marrero$filename), ]
marrero <- tibble::rowid_to_column(marrero, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > marrero$gloss_childes, 'DIFFERENT',
#                             ifelse(spanish_w$transcript == marrero$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="Marrero")

marrero_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(marrero)

final_marrero<- spanish %>% left_join(marrero_childes)

################################

#GRERLI

transcript <- get_transcripts(corpus="GRERLI")

spanish_w <- spanish_word  %>% filter(corpus_name=="GRERLI")
spanish_w <- spanish_w[order(spanish_w$transcript), ] # %>%   ###remove extra sessions
 # filter(transcript != "BecaCESNo/03f04.xml" )  %>%   ###remove extra sessions
 # filter(transcript != "BecaCESNo/07f07.xml" )
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

grerli <- childes %>% filter(corpus_name=="GRERLI") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
grerli <- grerli[order(grerli$filename), ]
grerli <- tibble::rowid_to_column(grerli, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > becacesno$gloss_childes, 'DIFFERENT',
#   ifelse(spanish_w$transcript == becacesno$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="GRERLI")

grerli_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(grerli)

final_grerli <- spanish %>% left_join(grerli_childes)

##################################

#Hess

transcript <- get_transcripts(corpus="Hess")

spanish_w <- spanish_word  %>% filter(corpus_name=="Hess")
spanish_w <- spanish_w[order(spanish_w$transcript), ]  %>%   ###remove extra sessions
 filter(transcript != "Hess/m12a2ex4.xml" )  %>%   ###remove extra sessions
 filter(transcript != "Hess/m9o1ex4.xml" )
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

hess <- childes %>% filter(corpus_name=="Hess") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
hess <- hess[order(hess$filename), ]
hess <- tibble::rowid_to_column(hess, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > becacesno$gloss_childes, 'DIFFERENT',
#   ifelse(spanish_w$transcript == becacesno$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="Hess")

hess_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(hess)

final_hess <- spanish %>% left_join(hess_childes)

####################################

transcript <- get_transcripts(corpus="ColMex")

spanish_w <- spanish_word  %>% filter(corpus_name=="ColMex")
spanish_w <- spanish_w[order(spanish_w$transcript), ] # %>%   ###remove extra sessions
 # filter(transcript != "Hess/m12a2ex4.xml" )  %>%   ###remove extra sessions
#  filter(transcript != "Hess/m9o1ex4.xml" )
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

colmex <- childes %>% filter(corpus_name=="ColMex") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
colmex <- colmex[order(colmex$filename), ]
colmex <- tibble::rowid_to_column(colmex, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > becacesno$gloss_childes, 'DIFFERENT',
#   ifelse(spanish_w$transcript == becacesno$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="ColMex")

colmex_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(colmex)

final_colmex <- spanish %>% left_join(colmex_childes)

#######################################

#  Remedi

transcript <- get_transcripts(corpus="Remedi")

spanish_w <- spanish_word  %>% filter(corpus_name=="Remedi")
spanish_w <- spanish_w[order(spanish_w$transcript), ]  %>%   ###remove extra sessions
 filter(transcript != "Remedi/020904.xml" )  %>%   ###remove extra sessions
 filter(transcript != "Remedi/021000.xml" )

spanish_w <- spanish_w[-c(7077:7080), ]
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

remedi <- childes %>% filter(corpus_name=="Remedi") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
remedi <- remedi[order(remedi$filename), ]
remedi <- tibble::rowid_to_column(remedi, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > remedi$gloss_childes, 'DIFFERENT',
#   ifelse(spanish_w$transcript == remedi$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="Remedi")

remedi_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(remedi)

final_remedi <- spanish %>% left_join(remedi_childes)

##############################

#SerraSole

transcript <- get_transcripts(corpus="SerraSole")

spanish_w <- spanish_word  %>% filter(corpus_name=="SerraSole")
spanish_w <- spanish_w[order(spanish_w$transcript), ]  #%>%   ###remove extra sessions
 # filter(transcript != "Remedi/020904.xml" )  %>%   ###remove extra sessions
#  filter(transcript != "Remedi/021000.xml" )

#spanish_w <- spanish_w[-c(7077:7080), ]
spanish_w <- tibble::rowid_to_column(spanish_w, "matching_index")

serrasole <- childes %>% filter(corpus_name=="SerraSole") %>% left_join(transcript) %>%
  mutate(filename = gsub("Spanish/", "", filename)) %>%
  rename(gloss_childes=gloss)
serrasole <- serrasole[order(serrasole$filename), ]
serrasole <- tibble::rowid_to_column(serrasole, "matching_index")

#spanish_w$gloss_ann <-ifelse(spanish_w$gloss > remedi$gloss_childes, 'DIFFERENT',
#   ifelse(spanish_w$transcript == remedi$filename, '', 'DIFFERENT'))  ###check matching

spanish <- spanish_base %>%
  filter(corpus_name=="SerraSole")

serrasole_childes <- spanish_w %>%
  rename(morph_id=id ) %>%
  select(-corpus_name ) %>%
  distinct() %>%
  left_join(serrasole)

final_serrasole <- spanish %>% left_join(serrasole_childes)

############################
#
spanish_morphology <- do.call("rbind", list(final_serrasole, final_remedi, final_colmex, final_hess, final_grerli, final_marrero, final_fernaguado, final_aguirre)) %>%
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


tokens_esp <- get_tokens(language = "spa", token="*", role_exclude = "Target_Child")
tokens_esp_ <- tokens_esp %>%
  left_join(spanish_morphology)

saveRDS(tokens_esp_, "~/Documents/morphological_predictors/spanish_morph.rds")

