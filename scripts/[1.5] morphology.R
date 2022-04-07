compute_verb_frame_df <- function(metric_data){
  lang <- metric_data$language[1]
  morph_file <- file.path(childes_path, glue("morph_{lang}.rds"))
  morph <- readRDS(morph_file) |>
    mutate(next_pos = NA,
           next_pos2 = NA,
           next_pos3 = NA)
  morph$next_pos[morph$utterance_id == lead(morph$utterance_id, n=1, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph$next_pos2[morph$utterance_id == lead(morph$utterance_id, n=2, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph$next_pos3[morph$utterance_id == lead(morph$utterance_id, n=3, default=NA) & !is.na(morph$utterance_id)] <- "same_utt"
  morph <- morph |>
    mutate(next_pos = ifelse(next_pos=="same_utt",
                             lead(pos, n=1, default=NA), NA),
           next_pos2 = ifelse(next_pos2=="same_utt",
                              lead(pos, n=2, default=NA), NA),
           next_pos3 = ifelse(next_pos3=="same_utt",
                              lead(pos, n=3, default=NA), NA)) |>
    mutate(next_pos = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int",
                             next_pos, ""),
           next_pos2 = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int",
                              next_pos2, ""),
           next_pos3 = ifelse(grepl('^v', pos) & pos!="v:aux" & pos!="v:int",
                              next_pos3, ""))

  morph$next_pos <- paste(morph$next_pos, morph$next_pos2,
                          morph$next_pos3, sep="_")
  morph <- morph |> select(-next_pos2, -next_pos3)

  np <- c('^neg_n:prop', '^pro:dem_', '^n:prop', '^pro_pro:dem',
          '^det:art_', '^det:gen_n', '^co_NA', '^n_n:prop', '^det:dem_pro:rel',
          '^det:poss_n', '^det:art_adj', '^pro:dem_adv')
  pp <- c("^prep_det:art", "^pro:y_n:prop", "^prep_n", "^adv_prep")
  s <- c("^v_prop:dem", "^pro:obj_v", "^adv_part", "^v_prep", "^v:mdl_",
         "^v_NA", "^part_v:mdl", "^v_co_", "v_pro")
  adv_empty <- c("^adv:place", "^adv_pro:dem", "^adv_conj_")

  morph$frame <- NA
  morph$frame <- ifelse(grepl(paste(np, collapse = "|"), morph$next_pos),
                        'NP', NA)
  morph$frame <- ifelse(grepl(paste(pp, collapse = "|"), morph$next_pos),
                        'PP', morph$frame)
  morph$frame <- ifelse(grepl(paste(s, collapse = "|"), morph$next_pos),
                        'S', morph$frame)
  morph$frame <- ifelse(grepl(paste(adv_empty, collapse = "|"), morph$next_pos),
                        'ADV_EMPTY', morph$frame)

  morph |>
    filter(!is.na(frame))
}


compute_verb_frame <- function(metric_data){
  print("Computing verb frames")
  file_u <- file.path(childes_path, glue("verb_frames_{childes_lang}.rds"))
  if (file.exists(file_u)) {
    morph <- readRDS(file_u)
  } else {
    morph <- compute_verb_frame_df(metric_data)
    saveRDS(morph, file_u)
  }
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
  a <- final |>
    group_by(token, pos) |>
    filter(per == max(per)) |>
    select(token, frame) |>
    distinct() |>
    rename(main_frame = frame)
  final |>
    group_by(token, pos) |>
    summarise(n_distinct_frame = n_distinct(frame), per_frame = max(per)) |>
    left_join(a[!duplicated(a$token), ]) |>
    group_by(token) |>
    summarise(n_distinct_frame = mean(n_distinct_frame, na.rm=TRUE),
              per_frame = mean(per_frame,  na.rm=TRUE),
              main_frame = paste(main_frame, collapse=","),)
}


compute_n_sfx_cat <-function(metric_data){
  print("Computing number of morphemes and categories")
  file_u <- file.path(childes_path, glue("n_sfx_cat_{childes_lang}.rds"))
  if (file.exists(file_u)) {
    morph <- readRDS(file_u)
  } else {
    l<- metric_data$language[1]
    file_ <- file.path(childes_path, glue("morph_{l}.rds"))
    morph <- readRDS(file_)
    morph$number.of.sfx <- str_remove(morph$affix_type_m, "sfxf")
    morph$number.of.sfx <- str_count(morph$number.of.sfx, "sfx")
    morph_ <- metric_data |>
      left_join(morph) |>
      mutate(n_affix = ifelse(affix_m=="NULL", NA, lengths(as.list(affix_m)))) |>
      group_by(token, pos) |>
      summarise(n_category = mean(n_affix, na.rm = TRUE),
                token_cats = list(unique(na.omit(affix_m))),
                n_sfx = mean(number.of.sfx, na.rm = TRUE)) |>
      filter(!is.na(n_category)) |>
      group_by(token) |>
      summarise(token_morphemes = paste(token_morphemes, collapse=","),
                n_sfx = mean(n_sfx,  na.rm=TRUE),
                n_category = mean(n_category,  na.rm=TRUE),
                pos = paste(pos, collapse=","))
    saveRDS(morph_, file_u)
    print(morph_)
  }
}


compute_n_type <- function(metric_data){
  print("Computing number of types")
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
  morph <- readRDS(file_)
  tmp <- metric_data |>
    left_join(morph)  |>
    select(token, stem_m, pos)
  a <- tmp |>
    distinct() |>
    group_by(stem_m, pos) |>
    summarise(token_types = list(unique(token))) |>
    mutate(n_type = lengths(token_types)) |>
    filter(!is.na(stem_m)) |>
    filter(!stem_m =="")
  tmp |>
    left_join(a) |>
    mutate(token_types = ifelse(token_types == "NULL", NA, token_types)) |>
    group_by(token, pos) |>
    summarise(n_type = mean(n_type, na.rm=TRUE),
              token_types = list(unique(token_types)),
              token_stem = list(unique(na.omit(stem_m)))) |>
    filter(!is.na(n_type)) |>
    group_by(token) |>
    summarise(n_type = mean(n_type,  na.rm=TRUE),
              token_types = paste(token_types, collapse=","),
              token_stem = paste(token_stem, collapse=","))

}

compute_prefix <- function(metric_data){
  print("Computing prefixes")
  l<- metric_data$language[1]
  file_ <- file.path(childes_path, glue("morph_{l}.rds"))
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
