# library(data.table)
source("scripts/stemmer.R")
childes_path <- "data/childes"



convert_lang_childes <- function(x){
   x <- substr(x, start = 1, stop = 3) %>% tolower() 
   pat <- c("jap", "cro", "man", "can", "dut", "gre", "ses", "slo", "far", 
              "ser", "ber", "fre")
   replace <- c("jpn", "hrv", "zho", "yue", "nld", "ell", "sot", "slv", 
                  "fas", "srp", "taq", "fra")
   for(i in seq_along(pat)) x<- gsub(pat[i], replace[i], x)
   return(x)
}



get_childes_metrics <- function( lang = NULL,
                   uni_lemmas =NULL,
                   corpus = NULL,
                   speaker_role = NULL, 
                   speaker_role_exclude = "Target_Child", 
                   target_child = "",
                   child_age = NULL, 
                   child_sex = NULL, 
                   pos = NULL, 
                   word = "",
                   freq=TRUE, 
                   uttlength=TRUE, 
                   charlen = TRUE, 
                   order = TRUE,
                   clean = FALSE){
  

norm_lang <- normalize_language(lang)  
file_ <- file.path(childes_path, glue("childes_metrics_{norm_lang}.csv"))
file_u <- file.path(childes_path, glue("unilemma_metrics_{norm_lang}.csv"))

if(!file.exists(file_))
  {  
print(glue("{file_} doesn't exist. Retrieving data from CHILDES..."))
   
  args_<-list(convert_lang_childes(lang), corpus, speaker_role, 
speaker_role_exclude, target_child, child_age, child_sex, pos, word, 
clean)
  data_<- do.call(get_data, args_)
  
  metrics <- data.frame(matrix(ncol=1,nrow=0, dimnames=list(NULL, c("gloss")))) %>%
    mutate(gloss = as.character(gloss))
  
 total <- nrow(data_$tokens)
 if (freq == TRUE){ metrics <- full_join(metrics, 
frequency(data_$tokens, total)) }
  
 if (uttlength == TRUE){ metrics <- full_join(metrics, 
mlu(data_$utterances, data_$tokens)) }
  
 if (order == TRUE){ metrics <- full_join(metrics, 
find_order(data_$utterances, data_$tokens)) }
  
 if (charlen == TRUE){ 
   metrics <- metrics %>%
     mutate(charactercount = str_count(gloss))}
  
 childes_metrics <- metrics %>% 
    distinct() %>%
      mutate(language = lang) %>%
         mutate(totalcount =  total) %>%
            rename(word = gloss)
 
 write_feather(childes_metrics, file.path(childes_path, glue("childes_metrics_{norm_lang}.csv")))
} else { 
  childes_metrics <- read_feather(file_)  
  print(glue("{file_} exists. Retrieving data from file"))
}  

##################### UNILEMMAS:

  if(!file.exists(file_u))
  {  
    print(glue("{file_u} doesn't exist. Retrieving unilemmas..."))
    unilemma_metrics<-prepare_unilemmas(lang, uni_lemmas)
    write_feather(unilemma_metrics, file.path(childes_path, glue("unilemma_metrics_{norm_lang}.csv")))
    
 } else {
   print(glue("{file_u} exists. Retrieving data from file"))
   unilemma_metrics <- read_feather(file_u)  
 }
 return(unilemma_metrics)  
}  


get_data <- function(lang = NULL,
                   corpus = NULL,
                   speaker_role = NULL, 
                   speaker_role_exclude = "Target_Child", 
                   target_child = "",
                   child_age = NULL, 
                   child_sex = NULL, 
                   pos = NULL,
                   word,
                   clean = FALSE)
                   {
  
norm_lang <- normalize_language(lang)  
  utterances <- childesr::get_utterances(language = lang, corpus = corpus, role = 
speaker_role, role_exclude = speaker_role_exclude, age = child_age, sex 
= child_sex) %>%
  mutate(gloss = tolower(gloss)) 
 # write_csv(utterances, file.path(childes_path, glue("childes_utterances_{norm_lang}.csv")))
 
  tokens_ <- childesr::get_tokens(language = lang, corpus = corpus, role = 
speaker_role, role_exclude = speaker_role_exclude, age = child_age, sex 
= child_sex, token="*") 

 if (word != ""){
  tokens <- childesr::get_tokens(language = lang, corpus = corpus, role = 
speaker_role, role_exclude = speaker_role_exclude, age = child_age, sex 
= child_sex, part_of_speech = pos, token = word)
} else {tokens <- tokens_ }
 tokens <- tokens %>%
  mutate(gloss = tolower(gloss))
# write_csv(tokens, file.path(childes_path, glue("childes_tokens_{norm_lang}.csv")))
  
if (target_child != ""){
  tokens <- tokens %>%
    filter(target_child_name == target_child)
} else {}

return(list(utterances = tibble(utterances), 
            tokens = tibble(tokens))) 
}


frequency<-function(tokens, total){ 
print(glue("Measuring frequency..."))
tokens_grouped <- tokens %>%
    select(gloss)  %>% 
      group_by(gloss) %>%
      count()  %>% 
        rename(wordcount = n) 
tokens_divide <- tokens %>% 
    left_join(tokens_grouped) %>%
      mutate(frequency_word=wordcount) %>%
        select(gloss, wordcount, frequency_word)
return(tokens_divide)
} 
space_clitics <- function(gloss){
  gloss <- gsub("'", "' ", gloss)
  gloss <- gsub("  ", " ", gloss)     
return(gloss) 
}

mlu <- function(utterances, tokens){
print(glue("Measuring mean utterance length..."))
utterances_mlu<- utterances %>%
  mutate(gloss = space_clitics(gloss)) %>%
    mutate(utt_length = sapply(strsplit(utterances$gloss, " "), length)) %>%
     select(id, utt_length) %>%  
      rename(utterance_id = id)
tokens_mlu <- tokens %>% 
    left_join(utterances_mlu) %>%
    select(gloss, utt_length) %>%
     group_by(gloss)  %>% 
      summarise(mlu_word = mean(utt_length)) %>% 
        select(gloss, mlu_word)
return(tokens_mlu)
}

find_positions <- function(tokens_pos){
print(glue("Finding positions in utterance..."))
tokens_pos_ <- tokens_pos %>%
  mutate(utt_length = sapply(strsplit(tokens_pos$sentence, " "), 
length)) %>%
  mutate(lastword=word(sentence, -1)) %>%
  mutate(firstword=word(sentence, 1)) %>% 
  mutate(order = ifelse(utt_length == 1, "solo", 
ifelse(as.character(lastword) == 
as.character(gloss),"last",ifelse(as.character(firstword) == 
as.character(gloss), "first", "other")))) 
return(tokens_pos_)  
}
coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2], df[3], df[4]))
}
find_order <- function(utterances, tokens){
utterances_pos <- utterances %>% 
    select(id, gloss) %>%
      mutate(gloss = space_clitics(gloss)) %>%
        rename(sentence = gloss) %>%
        rename(utterance_id = id)
tokens<- tokens %>%
  left_join(utterances_pos) %>%
    select(id, gloss, sentence)
tokens_pos <- find_positions(tokens)
tokens_count_ <- tokens_pos  %>% 
  count(gloss) %>% 
    rename(wordcount=n)
tokens_final <- tokens_pos  %>%
  count(gloss, order) %>%
    rename(ordercount=n) %>% 
      left_join(tokens_count_) %>%
        mutate(solo_word = ifelse(as.character(order) == 
"solo",ordercount,NA)) %>%
        mutate(final_word = ifelse(as.character(order) == "last",ordercount, 
NA)) %>%
        mutate(initial_word = ifelse(as.character(order) == "first",ordercount, 
NA)) %>%
        select(gloss, solo_word, final_word, initial_word) %>%
          group_by(gloss) %>% 
            summarise_all(coalesce_by_column)
return(tokens_final)
}


convert_lang_stemmer <- function(x){
  x <- normalize_language(x)
  x <- gsub("[()]","",as.character(x))
  pat <- c("spanish_mexican", "french_quebecois", "english_american")
  replace <- c("spanish", "french", "english")
  for(i in seq_along(pat)) x<- gsub(pat[i], replace[i], x)
  return(x)
}


#adapted from mikabr/aoa_prediction
transforms_fr <- c(
  function(x) gsub("(.*) \\(.*\\)", "\\1", x),
  function(x) gsub(" ", "_", x),
  function(x) gsub(" ", "+", x),
  function(x) gsub("(.+) \\1", "\\1", x),
  function(x) paste0(x, "e+moi"),
  function(x) paste0(x, "e-moi"),
  function(x) paste0(x, "+moi"),
  function(x) paste0(x, "-moi"),
  function(x) paste0(x, "ent"),
  function(x) paste0(x, "e-l"),
  function(x) paste0(x, "e-toi"),
  function(x) paste0(x, "-l"),
  function(x) paste0(x, "-toi"),
  function(x) paste0(x, "es-tu"),
  function(x) paste0(x, "s+tu"),
  function(x) paste0(x, "s-moi")
)


apply_transforms <- function(str, class_) {
  if (class_=="verbs") {transforms_fr %>% map_chr(~.x(str), )} else{}
}

special_case_files <- list.files(file.path(childes_path, "special_cases"), full.names = TRUE)
special_case_map <- map_df(special_case_files, function(case_file) {
  
  lang <- basename(case_file) %>% str_remove(".csv")
  special_cases <- read_csv(case_file, col_names = FALSE)
  map_df(1:nrow(special_cases), function(i) {
   uni_lemma <- special_cases$X1[i]
   lexical_class <- special_cases$X2[i]
    options <- special_cases[i, 3:ncol(special_cases)] %>%
     as.character() %>%
      discard(is.na)
      trans_opts <- map(options, apply_transforms, class_=lexical_class) %>% unlist() %>% 
       unique() #apply transforms to special cases
        data_frame(language = normalize_language(lang),
               uni_lemma = rep(uni_lemma, 2 * length(trans_opts)),
               stem = c(trans_opts, stem(trans_opts, convert_lang_stemmer(lang))))
  })
}) 

apply_transforms <- function(str ){ 
 transforms_fr %>% map_chr(~.x(str), )
}

lexical_classes<-c("function_words", "adjectives", "other", "nouns",  "verbs") 


load_unilemmas_main <- function(uni_lemmas) {
  map_df(lexical_classes, load_unilemmas, unilemmas=uni_lemmas) %>% 
  rbind() %>% 
  distinct()
}

load_unilemmas <- function(class_, unilemmas){
  unilemma <- unilemmas %>% 
    unnest(items) %>%
    filter(lexical_class==class_) %>%
    select(language, uni_lemma, definition) %>%
    distinct() %>%
    rename(words = definition)
pattern_map <- unilemma %>%
  split(paste(.$language, .$uni_lemma, .$words)) %>%
  map_df(function(uni_data) {
    language <- uni_data$language %>% normalize_language()
    uni_lemma <- uni_data$uni_lemma
    options <- uni_data$words %>% strsplit(", ") %>% unlist() %>% strsplit("/") %>% unlist()
    options <- c(options, stem(options, convert_lang_stemmer(language))) %>% unique() 
    #stemming with Snowball
    trans_opts <- map(options, apply_transforms, class_) %>% unlist() %>% unique()
    trans_opts <- c(trans_opts, stem(trans_opts, convert_lang_stemmer(language))) %>% unique()
    data_frame(language = rep(uni_data$language %>% normalize_language(), length(trans_opts)),
               uni_lemma = rep(uni_lemma, length(trans_opts)),
               stem = trans_opts)
  })

case_map <- bind_rows(special_case_map, pattern_map) %>% distinct()
return(case_map)
}

load_childes_data <- function(lang, uni_lemmas) {
  norm_lang <- normalize_language(lang)
  df_by_word <- read_feather(glue("data/childes/childes_metrics_{norm_lang}.csv")) %>%
    filter(!is.na(word)) %>%
    mutate(stem = stem(word, convert_lang_stemmer(lang))) %>%
    full_join(load_unilemmas_main(uni_lemmas) %>% filter(language == norm_lang), by = "stem") %>%
    rename(language = language.x) %>%
    group_by(uni_lemma, language) %>%
    filter(!is.na(uni_lemma), !is.na(word)) %>%
    group_by(word) 
  df_by_unilemma <- df_by_word %>% 
    group_by(uni_lemma) %>%
    summarise(nb_realisations_lemma=n(),
              words_lemma = list(unique(word)),
              sum_wordcount_lemma = sum(wordcount, na.rm = TRUE),
              mean_character_count_lemma = mean(charactercount, na.rm = TRUE), 
              frequency=sum(frequency_word, na.rm =TRUE),
              MLU=mean(mlu_word, na.rm = TRUE),
              solo_frequency=sum(solo_word, na.rm = TRUE),
              final_frequency=sum(final_word, na.rm = TRUE),     
              initial_frequency=sum(initial_word, na.rm = TRUE))
  print(glue("Grouping predictors across uni_lemmas..."))  
  df_total <- left_join(df_by_word, df_by_unilemma) 
  return(df_total)
  }  
    
prepare_unilemmas <- function(lang, uni_lemmas){
  norm_lang <- normalize_language(lang)
  childes_data <- load_childes_data(lang, uni_lemmas)
  childes_data$words_lemma <- vapply(childes_data$words_lemma, paste, collapse = ", ", character(1L))
  childes_data<- childes_data %>%
   select(-c(language.y))
  write_feather(childes_data,
          file.path(childes_path, glue("unilemma_metrics_{norm_lang}.csv")))
  return(childes_data)
}
