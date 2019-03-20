library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)
library(randomForest)
library(rpart)
library(rpart.plot)

###############################
# Pre-processamento dos dados #
###############################


#all_data = read_rds("clean_data.rds")
all_data = read_rds("clean_interpol_data.rds")

data_summary = all_data %>%
  # Retirar ids: 4, 9, 13 e 14. Pior mesmo era o 14
  filter(!(patient %in% c(14, 19))) %>%
  filter(!is.na(oxy) & !is.na(deoxy)) %>%
  group_by(patient, video, channel) %>%
  summarise(oxy_m = mean(oxy, na.rm = TRUE),
            deoxy_m = mean(deoxy, na.rm = TRUE),
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
    select(patient, video, oxy_m, deoxy_m)
  colnames(new_data)[3:4] = paste(colnames(new_data)[3:4],
                                  ii, 
                                  sep = "_")
  data_summary_cols %<>% left_join(new_data,
                                   by = c("patient", "video")) 
}

write_rds(data_summary_cols, "data_summary_cols_teste19.rds")
data_summary_cols = read_rds("data_summary_cols_teste19.rds") 

colnames(data_summary_cols)

######################
# Regressao Logistica #
######################

data_summary_cols %<>% arrange(patient)

# pega o fold para cada patient:
foldid_p = 1 + ((order(data_summary_cols$patient) - 1) %/% 10)
nfolds_p = max(foldid_p)

colnames(data_summary_cols)
fm = quiz_grade~.
XX= data_summary_cols %>% select(-oxy_m_3) %>% model.matrix(fm, .)

YY = data_summary_cols %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

aux <- cv.glmnet(XX, YY, family = "binomial",
                 keep = TRUE, nfolds = nfolds_p,
                 foldid = foldid_p)
i <- which(aux$lambda == aux$lambda.min)
coefficients(aux, s = aux$lambda.min)

# Entender quais subjects o modelo erra mais
#erros = abs(aux$fit.preval[,i] - data_summary_cols$quiz_grade)
#dim(erros) = c(10, 16)
#colMeans(erros)

roc <- prediction(aux$fit.preval[,i], data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")


