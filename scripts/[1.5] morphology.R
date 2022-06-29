compute_verb_frame_df <- function(metric_data){

  l<- metric_data$language[1]
  file_ <-  glue("{childes_path}/morph_{l}.rds")
  morph <- readRDS(file_)
  morph <- morph |>
    distinct()
  morph <- morph[!duplicated(morph$id), ]

  morph <- morph %>% mutate(next_pos_ = NA, next_pos2_=NA, next_pos3_=NA)


  l1 = c("deu", "tur", "nld")
  if (l %in% l1){

    morph$next_pos_[morph$utterance_id == lag(morph$utterance_id, n=1, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
    morph$next_pos2_[morph$utterance_id == lag(morph$utterance_id, n=2, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
    morph$next_pos3_[morph$utterance_id == lag(morph$utterance_id, n=3, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"

    morph$next_pos <- shift(morph$pos, 1, type = "lag")
    morph$next_pos2 <- shift(morph$pos, 2, type = "lag")
    morph$next_pos3 <- shift(morph$pos, 3, type = "lag")

    morph <- morph %>%
      mutate(next_pos = ifelse(next_pos_=="same_utt",next_pos, NA),
             next_pos2 = ifelse(next_pos2_=="same_utt",next_pos2, NA),
             next_pos3 = ifelse(next_pos3_=="same_utt",next_pos3, NA ),
      )
  }else{
    morph$next_pos_[morph$utterance_id == lead(morph$utterance_id, n=1, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
    morph$next_pos2_[morph$utterance_id == lead(morph$utterance_id, n=2, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
    morph$next_pos3_[morph$utterance_id == lead(morph$utterance_id, n=3, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"

    morph$next_pos <- shift(morph$pos, 1, type = "lead")
    morph$next_pos2 <- shift(morph$pos, 2, type = "lead")
    morph$next_pos3 <- shift(morph$pos, 3, type = "lead")

    morph <- morph %>%
      mutate(next_pos = ifelse(next_pos_=="same_utt",next_pos, NA),
             next_pos2 = ifelse(next_pos2_=="same_utt",next_pos2, NA),
             next_pos3 = ifelse(next_pos3_=="same_utt",next_pos3, NA ),
      )

  }
  morph$next_pos <- paste(morph$next_pos,morph$next_pos2,morph$next_pos3,sep="_")
  morph <- morph %>% select(-next_pos2, - next_pos3)

  np <- c('^neg_n:prop', '^pro:dem_', '^n:prop', '^pro_pro:dem','^det:art_','^det:gen_n', '^co_NA','^n_n:prop','^det:dem_pro:rel', '^det:poss_n', '^det:art_adj', '^pro:dem_adv', '^ADJ', '^N_')
  pp <- c("^prep_det:art", "^pro:y_n:prop", "^prep_n", "^adv_prep")
  s <- c("^v_prop:dem", "^pro:obj_v", "^adv_part", "^v_prep", "^v:mdl_", "^v_NA", "^part_v:mdl", "^v_co_", "v_pro", '^V')
  adv_empty <- c("^adv:place", "^adv_pro:dem", "^adv_conj_")

  morph$frame <- NA
  morph$frame <- ifelse(grepl(paste(np, collapse = "|"), morph$next_pos),'NP',NA)
  morph$frame <- ifelse(grepl(paste(pp, collapse = "|"), morph$next_pos),'PP', morph$frame)
  morph$frame <- ifelse(grepl(paste(s, collapse = "|"), morph$next_pos),'S',morph$frame)
  morph$frame <- ifelse(grepl(paste(adv_empty, collapse = "|"), morph$next_pos),'ADV_EMPTY',morph$frame)

  morph |>
    filter(!is.na(frame))
}


compute_verb_frame <- function(metric_data){
  print("Computing verb frames")
    morph<-compute_verb_frame_df(metric_data)
  tmp_data <- metric_data |>
    select(token_id, token, utterance_id) |>
    left_join(morph)
  m <- tmp_data |>
    filter(!is.na(frame)) |>
    group_by(token, pos) |>
    summarise(c=n())
  final <- tmp_data |>
    filter(!is.na(frame)) |>
    group_by(token, frame, pos) |>
    summarise(tmp=n()) |>
    left_join(m) |>
    mutate(per= 100 *tmp/c)
  a <- final %>% group_by(token, pos) %>% filter(per == max(per)) %>%
    select(token, frame) %>% distinct() %>% rename(main_frame=frame)
  aa<- final |>
    group_by(token, pos) |>
    summarise(n_distinct_frame = n_distinct(frame), per_frame = max(per)) |>
    left_join(a[!duplicated(a$token), ]) |>
    group_by(token) |>
    summarise(n_distinct_frame = mean(n_distinct_frame, na.rm=TRUE),
              per_frame = mean(per_frame,  na.rm=TRUE),
              main_frame = paste(main_frame, collapse=","),)
  return(aa)
}

comma_sep = function(x) {
  x = strsplit(x, ",")
  as.list(lapply(x, paste, collapse = ','))
}

compute_n_sfx_cat <-function(metric_data){
  print("Computing number of morphemes and categories")
  l<- metric_data$language[1]
  print("morph file")
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_) |>
    rename(gloss= gloss_m)

  if("n_morpheme" %in% colnames(morph)){
  morph2 <- metric_data |>
    left_join(morph) |>
    group_by(gloss) |>
    summarise(n_cat = mean(n_cat, na.rm = TRUE),
              n_affix = mean(n_morpheme, na.rm = TRUE),
              pos = pos) |>
    filter(!is.na(pos)) |>
    rename(token = gloss) |>
    group_by(token) |>
    summarise(n_affix = mean(n_affix,  na.rm=TRUE),
              n_cat = mean(n_cat,  na.rm=TRUE),
              pos = paste(pos))
  } else {
  morph2 <- metric_data |>
      left_join(morph) |>
      group_by(gloss) |>
      summarise(n_cat = mean(n_cat, na.rm = TRUE),
                pos = pos) |>
      filter(!is.na(pos)) |>
      rename(token = gloss) |>
      group_by(token) |>
      summarise(n_cat = mean(n_cat,  na.rm=TRUE),
        pos = paste(pos))
  }
    morph2$pos = str_replace_all(morph2$pos,"V.PTCP","V")
    morph <- morph2 |> distinct()
    print(morph)
}


compute_n_type <- function(metric_data){
  print("Computing number of types")
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_) |>
    rename(gloss= gloss_m)
  tmp <- metric_data |>
    left_join(morph)  |>
    select(gloss, stem_m, pos)
  a <- tmp |>
    distinct() |>
    group_by(stem_m) |>
    summarise(token_types = list(unique(gloss))) |>
    mutate(n_type = lengths(token_types)) |>
    filter(!is.na(stem_m)) |>
    filter(!stem_m =="")
  last <- tmp |>
    left_join(a) |>
    group_by(gloss) |>
    summarise(n_type = mean(n_type, na.rm=TRUE),
              #token_types = list(unique(token_types)),
              token_stem = list(unique(na.omit(stem_m))),
              pos = pos) |>
    filter(!is.na(n_type)) |>
    distinct() |>
    group_by(gloss) |>
    summarise(n_type = mean(n_type,  na.rm=TRUE),
              #token_types = paste(token_types, collapse=","),
              token_stem = paste(token_stem, collapse=",")) |>
    rename(token = gloss)
  print(last)
}

compute_prefix <- function(metric_data){
  print("Computing prefixes")
  l<- metric_data$language[1]
  file_ <- glue("{childes_path}/morph_{l}.rds")
  morph <- readRDS(file_)
  metric_data |>
    left_join(morph) |>
    mutate(prefix = ifelse(prefix_m == "", 0, 1)) |>
    group_by(token, pos) |>
    summarise(prefix = mean(prefix, na.rm=TRUE)) |>
    filter(!is.na(prefix)) |>
    group_by(token)|>
    summarise(prefix = mean(prefix, na.rm=TRUE))
}
