library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)

#all_data = read_rds("clean_data.rds")
all_data = read_rds("clean_interpol_data.rds")

data_summary = all_data %>%
  filter(!is.na(oxy) & !is.na(deoxy)) %>%
  mutate(oxy = (oxy - mean(oxy))/sd(oxy),
         deoxy = (deoxy - mean(deoxy))/sd(deoxy)) %>% 
  group_by(patient, video, channel) %>%
  summarise(oxy_tv = mean(deoxy),
            quiz_grade = mean(quiz_grade)
  ) %>% 
  ungroup() %>% 
  mutate(patient = as.factor(patient),
         channel = as.factor(channel),
         video = as.factor(video)
  )

data_summary_cols = data_summary %>%
  filter(channel == 1) %>% 
  select(patient, video, quiz_grade)
for(ii in unique(data_summary$channel))
{
  new_data = data_summary %>% 
    filter(channel == ii) %>% 
    select(patient, video, oxy_tv)
  colnames(new_data)[3] = paste(colnames(new_data)[3],
                                  ii, 
                                  sep = "_")
  data_summary_cols %<>% left_join(new_data,
                                   by = c("patient", "video")) 
}

#Regressao Logistica
XX = model.matrix(quiz_grade~., data_summary_cols)
YY = data_summary_cols %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

aux <- cv.glmnet(XX, YY, family = "binomial",
                 keep = TRUE, nfolds=nrow(XX))
i <- which(aux$lambda == aux$lambda.min)
coefficients(aux, s = aux$lambda.min)

roc <- prediction(aux$fit.preval[,i], data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
data_roc_sd <- tibble("espec" = roc@x.values[[1]],
                   "sens" = roc@y.values[[1]],
                   "method"   = "GLMNET")

gsd <- data_roc_sd %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)
ggsave("./figuras/ROC-regressao_sd.pdf", plot = gsd)
gsd
