library(tidyverse)



morphnet_extract <- function(childes_lang){

  if (childes_lang == "spa"){
    unimorph1 <- read.csv("data/temp_saved_data/unimorph_spa1.tsv", sep="\t")
    colnames(unimorph1) <- c("stem","gloss","morph_info", "segment_info")

    unimorph2 <- read.csv("data/temp_saved_data/unimorph_spa2.tsv", sep="\t")
    colnames(unimorph2) <- c("stem","gloss","morph_info", "segment_info")

    unimorph <- dplyr::bind_rows(unimorph1, unimorph2)
    write.table(unimorph, "data/temp_saved_data/unimorph_spa.tsv", sep = "\t")
  }
  f <- file.path(glue("data/temp_saved_data/unimorph_{childes_lang}.tsv"))
  unimorph <- read.csv(f, sep = "\t")

  if (childes_lang == "eng"){
    unimorph <- head(unimorph, 100000)
  }

  d <- file.path(glue("data/temp_saved_data/unimorph_{childes_lang}_der.tsv"))
  if (!(childes_lang=="rus")){
  unimorph_dev <- read.csv(d, sep = "\t")
  }

  childes<- get_childes_data({childes_lang}, corpus_args)
  if (childes_lang == "eng"){
    childes[[1]] <- head(childes[[1]], 200000)
    childes[[2]] <- head(childes[[2]], 200000)
  }

  colnames(unimorph) <- c("stem","gloss","morph_info", "segment_info")
  childes <- childes$tokens |>
    rename(old_stem = stem)

unimorph$morph_info = str_replace(unimorph$morph_info,"^N;","N-")
unimorph$morph_info = str_replace(unimorph$morph_info,"^V;","V-")
unimorph$morph_info = str_replace(unimorph$morph_info,"^ADJ;","ADJ-")
unimorph$morph_info = str_replace(unimorph$morph_info,"^V\\|","V-")
unimorph$morph_info = str_replace(unimorph$morph_info,"^ADJ\\|","ADJ-")
unimorph$morph_info = str_replace(unimorph$morph_info,"^N\\|","N-")
unimorph$morph_info = str_replace_all(unimorph$morph_info,"\\|",";")
unimorph$morph_info = str_replace_all(unimorph$morph_info," ",";")
unimorph$morph_info = str_replace_all(unimorph$morph_info,";$","")
unimorph$morph_info = str_replace_all(unimorph$morph_info,"^ADP\\+DET","ADP+DET-")

unimorph <- unimorph |>
  separate(morph_info, c("pos","morph_info"), sep = "-")

unimorph <- unimorph |>
  mutate(n_morpheme = ifelse(segment_info=="", NA, lengths(as.list(strsplit(segment_info, "\\|")))))

unimorph<- unimorph |>
  mutate(n_cat = ifelse(morph_info=="", NA, lengths(as.list(strsplit(morph_info, ";")))))

corpus <- childes|>
  left_join(unimorph)

corpus<- corpus |>
  group_by(id, utterance_id, corpus_name, gloss) |>
  summarise(stem_m = stem, #paste(sort(unique(stem)),collapse=", "),
            gloss = gloss, #paste(sort(unique(gloss)),collapse=", "),
            affix_m = morph_info, #paste(unique(sort(unique(morph_info))),collapse="\\/"),
            morpheme_m = segment_info,# paste(unique(sort(unique(segment_info))),collapse="\\|"),
            pos = pos,
            n_morpheme = mean(n_morpheme),
            n_cat  = mean(n_cat),
            prefix_m = prefix)|>
  distinct()


corpus["affix_type_m"] = ""
corpus["prefix_m"] = ""

corpus <- corpus |>
  distinct()

corpus['der_morpheme'] <- NA

if (!(childes_lang == "rus")){

colnames(unimorph_dev) <- c("stem","gloss","pos", "pos1", "der_morpheme" ,"prefix")

unimorph_dev<- unimorph_dev |>
  mutate(der_morpheme = ifelse(der_morpheme=="", NA, TRUE))

unimorph_dev<- unimorph_dev |>
  mutate(prefix = ifelse(prefix=="prefix", TRUE, NA))

corpus <- corpus|>
  left_join(unimorph_dev)
}

corpus <- corpus |>
  mutate(n_morpheme = ifelse(is.na(der_morpheme), n_morpheme , n_morpheme+1))

corpus$stem_m <- coalesce(corpus$stem, corpus$stem_m)

corpus <- corpus |>
  select(id, utterance_id, corpus_name,gloss, n_cat, pos, stem_m, affix_m, affix_type_m, prefix, prefix_m, n_morpheme, morpheme_m, der_morpheme)

o <- file.path(glue("data/childes/morph_{childes_lang}.rds"))

saveRDS(corpus, o)
return(corpus)
}


##########################################


unimorph_extract <- function(childes_lang){
  f <- file.path(glue("data/temp_saved_data/unimorph_{childes_lang}.csv"))
unimorph <- read.csv(f, sep = "\t" ) #remove sep = "\t" for Norwegian data

childes<- get_childes_data({childes_lang}, corpus_args)
colnames(unimorph) <- c("stem","gloss","morph_info")
childes <- childes$tokens |>
  rename(old_stem = stem)

unimorph$morph_info = str_replace(unimorph$morph_info,";","/")

corpus <- childes |>
  left_join(unimorph) |>
  separate(morph_info, c("pos","suffix"), sep = "/")
corpus$suffix = str_replace_all(corpus$suffix,";",",")
corpus$suffix = str_replace_all(corpus$suffix," ","")

corpus <- corpus |>
  filter(!is.na(suffix))

corpus <- corpus |>
  group_by(id, utterance_id, corpus_name, gloss) |>
  summarise(stem_m = stem,
            #stem_m = paste(sort(unique(stem)),collapse=", "),
            gloss = gloss, #paste(sort(unique(gloss)),collapse=", "),
            affix_m = suffix,
            pos = pos)
corpus$affix_m = str_replace_all(corpus$affix_m," ","")
corpus$pos = str_replace_all(corpus$pos," ","")

corpus["affix_type_m"] = ""
corpus["prefix_m"] = ""


corpus <- corpus %>%
  #mutate(affix_m=unique(unlist(paste(unlist(str_split(affix_m,',')),collapse=',')))) |>
  mutate(n_cat = ifelse(affix_m=="", NA, lengths(as.list(strsplit(affix_m, ","))))) |>
  mutate(pos_m=unique(unlist(paste(unique(unlist(str_split(pos,','))),collapse=','))))
corpus <- corpus |>
  group_by(id, utterance_id, corpus_name, gloss) |>
  summarise(n_cat = mean(n_cat),
            pos = pos,
            affix_m = unique(unlist(paste(unlist(str_split(affix_m,',')),collapse=','))),
            stem_m = stem_m,
            affix_type_m = affix_type_m,
            prefix_m = prefix_m) |>
  distinct()

o <- file.path(glue("data/childes/morph_{childes_lang}.rds"))

saveRDS(corpus, o)
return(corpus)
}
