#####fitted_aoa_models
predictors<-c("mlu", "babiness", "concreteness", "valence", "solo_frequency", "first_frequency", "final_frequency", "frequency", "length_char", "n_tokens")
preds_=list(c("frequency","mlu","final_frequency","valence", "concreteness", "babiness", "first_frequency", "solo_frequency", "length_char", "n_tokens"))


coefs_by_lang<- function(lang_coefs){

#fitted_aoa_models <- readRDS("data/fitted_aoa_models_whole1.rds")

#term = fitted_aoa_models$results[[1]][1]
#estimate = fitted_aoa_models$results[[1]][2]

ggplot(lang_coefs, aes(x = estimate, y = term)) +
  facet_grid(language~measure, scales = "free") +
  geom_pointrange(aes(colour = term, shape = signif,
                       xmin = estimate - 1.96 * std.error,
                       xmax = estimate + 1.96 * std.error))+
  scale_shape_manual(values = c(19, 21), guide = "none") +
  labs(y = "", x = "Coefficient estimate")+
  theme(legend.position="none")
}

coefs_by_pred<- function(lang_coefs){
lang_coefs_1 <- lang_coefs %>%
  group_by(measure, term) %>%
    summarize(mean=mean(estimate))
lang_coefs <- lang_coefs %>% left_join(lang_coefs_1)

ggplot(lang_coefs, aes(x = estimate, y = term, colour = language)) +
  facet_grid(~measure, scales = "free")+
  geom_point(aes(shape = signif), size = 3, alpha = 0.8) +
  geom_point(data=lang_coefs_1,  mapping=aes(x = mean, y = term), col="black",size = 3, shape = 3, fill="black")+
  labs(x = "Coefficient estimate", y = "Predictors")
}


####### cross-validation across lang
cv_dev_lang<- function(cv_across_lang ){

cv_abs_dev <- cv_across_lang %>%
 # filter(pr %in% predictors) %>%
  group_by(language, measure) %>%
  arrange(mean_abs_dev, .by_group = TRUE)
#cv_abs_dev$pr <- as.factor(unlist(cv_abs_dev$pr))

ggplot(cv_abs_dev, aes(x = mean_abs_dev, y = language)) +
  facet_grid(~measure, scales = "free") +
  geom_pointrange(aes(colour = language,
                      xmin = ci_mad_min,
                      xmax = ci_mad_max))+
  scale_shape_manual(values = c(19, 21), guide = "none") +
  labs(y = "", x = "Mean absolute deviance")+
  theme(axis.text.x = element_text(angle = 30))+
  theme(legend.position="none")
}

####### cross-validation across lang

cv_dev_lang_lex<- function(cv_across_lex ){
  cv_across_lex <- cv_across_lex %>%
  mutate(countper=count/50) %>%
  group_by(language, measure, lexical_category) %>%
  summarize(av_count_per=mean(countper))


ggplot(cv_across_lex, aes(y = language, x =av_count_per , fill=lexical_category)) +
  geom_bar(position="stack", stat="identity")+
  facet_grid(rows=vars(measure), scales = "free")+
  theme(axis.text.x = element_text(angle = 30))

}

############ reliability plot

relia_plot <- function(){
dfinal <- readRDS("data/reliabilities.rds")

ggplot(dfinal %>% filter(measure=="understands"), aes(y = r, x=language, fill=predictor)) +
  geom_bar(position="dodge", stat="identity") +
 # facet_grid(cols = vars(measure), rows = vars(language)) +
  geom_errorbar(position=position_dodge(width=0.9), aes(x=language, y=threshold_half, ymax=threshold_half, ymin=threshold_half,  color=predictor)) +
  theme(legend.position = "bottom") +
  ylab("R2") +
  xlab("Predictor") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 30))
}




