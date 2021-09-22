library(ggrepel)

#####fitted_aoa_models
predictors<-c("mlu", "babiness", "concreteness", "valence", "solo_frequency", "first_frequency", "final_frequency", "frequency", "length_char", "n_tokens")
preds_=list(c("frequency","mlu","final_frequency","valence", "concreteness", "babiness", "first_frequency", "solo_frequency", "length_char", "n_tokens"))


coefs_by_lang<- function(lang_coefs){
#term = fitted_aoa_models$results[[1]][1]
#estimate = fitted_aoa_models$results[[1]][2]
ggplot(lang_coefs, aes(x = estimate, y = term)) +
  facet_grid(rows = vars(measure), cols=vars(language), scales = "free") +
  geom_pointrange(aes(colour = term, shape = signif,
                       xmin = estimate - 1.96 * std.error,
                       xmax = estimate + 1.96 * std.error))+
  scale_shape_manual(values = c(19, 21)) +
  labs(y = "", x = "Coefficient estimate")+
  theme(legend.position="none")+
  xlim(-5,5) +
  theme_bw()+
    theme(axis.text.x = element_text(angle = 30))
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
  labs(x = "Coefficient estimate", y = "Predictors") +
  xlim(-4,4) +
  theme_bw()
}


####### cross-validation across languages
cv_dev_lang<- function(cv_across_lang ){
ggplot(cv_across_lang, aes(x = mean_abs_dev, y = language)) +
  facet_grid(~measure, scales = "free") +
  geom_pointrange(aes(colour = language,
                      xmin = ci_mad_min,
                      xmax = ci_mad_max))+
  scale_shape_manual(values = c(19, 21), guide = "none") +
  labs(y = "", x = "Mean absolute deviance (by month)")+
  #xlim(0,10) +
  #theme(axis.text.x = element_text(angle = 30))+
  theme(legend.position="none")+
  theme_bw()
}

####### cross-validation lexical category proportion
aoa_mad_lex_cat<- function(cv_across_lex, lang, meas, wb_data, preds ){

  cv_across <- cv_across_lex %>%
   # filter(pred_ %in% preds ) %>%
    group_by(lexical_category) %>%
    summarise(mad=n()) %>%
    mutate(mad=mad/sum(mad))

  eng_wb_data <- wb_data %>% filter(language ==lang, measure==meas) %>%
    unnest(items) %>%
    group_by(lexical_class) %>%
    summarise(aoa=n()) %>%
    mutate(aoa=aoa/sum(aoa)) %>%
    rename(lexical_category=lexical_class) %>%
    left_join(cv_across)

  mad_aoa <- eng_wb_data%>%
    pivot_longer(!lexical_category, names_to = "type", values_to = "count")

  ggplot(mad_aoa, aes(y = count, x =type, fill=lexical_category)) +
  geom_bar(stat="identity")
}


dev_words <- function(cv_across_lex){
eng_across_lang_lex2 <- cv_across_lex
ggplot(eng_across_lang_lex2, aes(y=aoa_pred , x=aoa, fill=lexical_category, color=lexical_category)) +
  geom_point(alpha = .5)+
  geom_smooth() +
  geom_text_repel(aes(label=test_word), max.overlaps = 20)+
  xlim(c(0,40))+ ylim(c(0,40))+
  geom_point(alpha = .1)+
  theme_bw()
}

############ reliability plot
pdf(file = "data/reliability_plot.pdf",
    width = 12,
    height = 3.5)


relia_plot <- function(dfinal, lang_list){
 ggplot(dfinal %>% filter(language %in% lang_list), aes(y = predictor, x=rsquared, fill=preds)) +
  geom_bar(position="dodge", stat="identity") +
  facet_grid(rows = vars(measure), cols=vars(language)) +
  geom_errorbar(position=position_dodge(width=0.9), aes(y=preds, x=threshold_half, xmax=threshold_half, xmin=threshold_half,  color=predictor)) +
  theme(legend.position = "bottom") +
  ylab("Predictor") +
  xlab("R2") +
  theme(legend.title = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))
}
dev.off()

coefs_by_pred_all<- function(all_coefs){
  all_coefs_1 <- all_coefs %>%
    group_by(measure, term, interact_lexcat) %>%
    summarise(mean=mean(estimate))
  all_coefs <- all_coefs   |>
  left_join(all_coefs_1)

  ggplot(all_coefs, aes(x = estimate, y = term, colour = language)) +
    facet_grid(interact_lexcat~measure, scales = "free")+
    geom_point(aes(shape = signif), size = 3, alpha = 0.8) +
    geom_point(data=all_coefs_1,  mapping=aes(x = mean, y = term), col="black",size = 3, shape = 3, fill="black")+
    labs(x = "Coefficient estimate", y = "Predictors") +
#    xlim(-4,4) +
    theme_bw()
}
