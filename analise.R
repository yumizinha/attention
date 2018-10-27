library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)

all_data = read_rds("clean_data.rds")
trechos = read.csv("trechos.csv")

# Função verifica intervalo do estimulo visual na função trechos
estimulo = function(rel_frame, video) {
  rel_frame >= trechos[video, 2] && rel_frame <= trechos[video, 3]
}

# Cria uma coluna chamada trecho no all_data atribuindo o retorno 
# da função estimulo (1) calculada nos pontos rel_frame e video:
# Obs: no filtro, no lugar de trecho == TRUE poderia colocar somente "trecho"
# que já entenderia a condição booleana
all_data %<>%
  mutate(trecho = map2_lgl(rel_frame, video, estimulo)) %>% 
  filter(trecho == TRUE) %>% 
  select(-trecho) # retira a coluna trecho

data_summary = all_data %>%
  filter(!is.na(oxy) & !is.na(deoxy)) %>% 
  group_by(patient, video, channel, quiz_grade) %>%
  summarise(oxy_m = mean(oxy, na.rm = TRUE),
            deoxy_m = mean(deoxy, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(patient = as.factor(patient),
         channel = as.factor(channel),
         video = as.factor(video)
  )

#Regressao Logistica
XX = model.matrix(quiz_grade~., data_summary)
YY = data_summary %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

aux <- cv.glmnet(XX, YY, family = "binomial",
                 keep = TRUE, nfolds=nrow(XX))
i <- which(aux$lambda == aux$lambda.min)
coefficients(aux, s = aux$lambda.min)
  

roc <- prediction(aux$fit.preval[,i], data_summary$quiz_grade)
roc <- performance(roc, measure = "tpr", x.measure = "fpr") 
data_roc <- tibble("espec" = roc@x.values[[1]],
                   "sens" = roc@y.values[[1]],
                   "method"   = "GLMNET")

g <- data_roc %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)
ggsave("./figuras/ROC-regressao_medias.pdf", plot = g)
