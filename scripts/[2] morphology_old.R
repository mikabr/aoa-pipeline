compute_verb_frame_df <- function(metric_data) {
  lang <- metric_data$language[1]
  verb_final_langs <- c("deu", "nld", "tur")
  shift_type = if (lang %in% verb_final_langs) "lag" else "lead"

  # maybe need to hand-code other languages
  np_opts <- c("^(neg_|n_)?n:prop",
               "^(pro_)?pro:dem",
               "^det:(art_|gen_|dem_pro:rel|poss_|dem_adv)",
               "^co_NA",
               "^ADJ",
               "^N_")
  pp_opts <- c("^prep_(det:art|n)",
               "^pro:y_n:prop",
               "^adv_prep")
  s_opts <- c("^v_(prop:dem|prep|co_|pro)",
              "^pro:obj_v",
              "^adv_part",
              "^(part)?v:mdl_",
              "^v_NA",
              "^part_v:mdl",
              "^V_")
  adv_opts <- c("^adv(:place|_pro:dem|_conj_)")

  frame_df <- metric_data |>
    mutate(
      next_pos_1 = ifelse(shift(utterance_id, 1, type = shift_type) == utterance_id,
                          shift(pos, 1, type = shift_type), NA),
      next_pos_2 = ifelse(shift(utterance_id, 2, type = shift_type) == utterance_id,
                          shift(pos, 2, type = shift_type), NA),
      next_pos_3 = ifelse(shift(utterance_id, 3, type = shift_type) == utterance_id,
                          shift(pos, 3, type = shift_type), NA),
      next_pos = paste(next_pos_1, next_pos_2, next_pos_3, sep = "_"),
      frame = ifelse(grepl(paste(adv_opts, collapse = "|"), next_pos), "ADV",
              ifelse(grepl(paste(s_opts, collapse = "|"), next_pos), "S",
              ifelse(grepl(paste(pp_opts, collapse = "|"), next_pos), "PP",
              ifelse(grepl(paste(np_opts, collapse = "|"), next_pos), "NP", NA))))
    )

  frame_df |> filter(!is.na(frame))
}

compute_verb_frames <- function(metric_data){
  print("Computing verb frames...")

  frame_df <- compute_verb_frame_df(metric_data)

  frame_props <- frame_df |>
    group_by(token, pos, frame) |>
    summarise(n_per_frame = n()) |>
    group_by(token, pos) |>
    mutate(frame_prop = 100 * prop.table(n_per_frame),
           n_frames = n_distinct(frame))

  main_frames <- frame_props |>
    group_by(token, pos) |>
    filter(frame_prop == max(frame_prop)) |>
    rename(main_frame = frame,
           main_frame_prop = frame_prop)

  main_frames |>
    group_by(token) |>
    summarise(n_frames = mean(n_frames, na.rm = TRUE),
              main_frame = paste(main_frame, collapse = ","),
              main_frame_prop = mean(main_frame_prop, na.rm = TRUE))
}

# comma_sep = function(x) {
#   x = strsplit(x, ",")
#   as.list(lapply(x, paste, collapse = ','))
# }

compute_n_cats <- function(metric_data) {
  print("Computing number of morphemes and categories...")

  metric_data |>
    group_by(token) |>
    summarise(n_morph_categories = mean(n_cat, na.rm = TRUE),
              n_affixes = mean(n_morpheme, na.rm = TRUE),
              pos = pos) |>
    distinct()
}

compute_n_forms <- function(metric_data) {
  print("Computing number of forms...")

  stem_forms <- metric_data |>
    select(token, stem_m, pos) |>
    distinct() |>
    group_by(stem_m) |>
    summarise(forms = token |> unique() |> list(),
              n_forms = forms |> lengths()) |>
    filter(!is.na(stem_m),
           stem_m != "")

  metric_data |>
    select(token, stem_m) |>
    left_join(stem_forms) |>
    group_by(token) |>
    summarise(forms = forms |> unique() |> list(),
              n_forms = mean(n_forms, na.rm = TRUE))
}

# compute_prefix <- function(metric_data) {
#   print("Computing prefixes...")
#
#   metric_data |>
#     mutate(prefix = ifelse(prefix_m == "", 0, 1)) |>
#     group_by(token, pos) |>
#     summarise(prefix = mean(prefix, na.rm=TRUE)) |>
#     filter(!is.na(prefix)) |>
#     group_by(token)|>
#     summarise(prefix = mean(prefix, na.rm=TRUE))
# }
