
get_vif_collinearity_scores <- function(df, predictors){
  formula <- glue("aoa ~ {paste(predictors, collapse = ' + ')}")  %>% as.formula()
  model <- lm(formula, data=df)
  vif_results = car::vif(model)
  df.vif <- as.data.frame(vif_results)
  df.vif <- tibble::rownames_to_column(df.vif, "predictors")

  return(df.vif)
}

get_vif_bylang_bymeasure<- function(df, predictors){
  grouped_vif <- df |>
    group_by(language, measure) |>
    group_nest() |>
    mutate(vif_results = map(.x = data, .f =~get_vif_collinearity_scores(.x, predictors))) |>
    unnest(vif_results)

  return(grouped_vif)
}
