
get_vif_collinearity_scores <- function(df, dependent_variable, predictors){
  formula <- glue("{dependent_variable} ~ {paste(predictors, collapse = ' + ')}")  %>% as.formula()
  model <- lm(formula, data=df)
  vif_results = car::vif(model)
  return(vif_results)
}
