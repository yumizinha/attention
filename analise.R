library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)
library(randomForest)

###############################
# Pre-processamento dos dados #
###############################

#all_data = read_rds("clean_data.rds")
all_data = read_rds("clean_interpol_data.rds")

data_summary = all_data %>%
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

# write_rds(data_summary_cols, "data_summary_cols.rds")

data_summary_cols = read_rds("data_summary_cols.rds")


##############################
# Modelinhos                 #
##############################
data_roc_mean = tibble()


###########
# Exemplo - Implementando Arvore simples
#########

# Ajuste da arvore. Acertos em função de oxy_m_1: 
ajuste = rpart(quiz_grade ~ oxy_m_1, data_summary_cols)

# Identificando o nível da árvore com melhor erro de validação cruzada:
melhor_nivel = which.min(ajuste$cptable[,"xerror"])

# Identifica cp associado ao melhor nivel:
cp = ajuste$cptable[melhor_nivel, "CP"]
ajuste %<>% prune(cp)
  
# É possível verificar que uma arvore só com oxy_m1 não é possível ver nada =)
# no ajuste o modelo escolhe somente a raiz (chutar acerto ou erro ao invés de usar a oxy)


###########
# Arvores #
###########

## Arvore com todo mundo
# Ajuste da arvore. Acertos em função de oxy_m_1: 
formula = quiz_grade~.
ajuste_todos = data_summary_cols %>% 
  select(-patient, -video) %>% 
  rpart(formula, .)

# Identificando o nível da árvore com melhor erro de validação cruzada:
melhor_nivel = which.min(ajuste_todos$cptable[,"xerror"])

# Identifica cp associado ao melhor nivel:
cp = ajuste_todos$cptable[melhor_nivel, "CP"]

ajuste_todos %<>% prune(cp) 
roc <- prediction(predict(ajuste_todos),
                  data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_rpart <- tibble("espec" = roc@x.values[[1]],
                              "sens" = roc@y.values[[1]],
                              "method"   = "RPART")
data_roc_mean %<>% rbind(data_roc_mean_rpart)


######################
# Floresta aleatoria #
######################

formula = quiz_grade~. 

# Classificação usa fatores, regressão usa números reais, por isso
# colocar em fator:
ajuste = data_summary_cols %>%
  mutate(quiz_grade = as.numeric(quiz_grade)) %>% 
  randomForest(formula, .)

roc <- prediction(ajuste$predicted, data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_rf <- tibble("espec" = roc@x.values[[1]],
                           "sens" = roc@y.values[[1]],
                           "method"   = "RF")
data_roc_mean %<>% rbind(data_roc_mean_rf)


######################
# Regressao Logistica #
######################

XX = model.matrix(quiz_grade~., data_summary_cols)
YY = data_summary_cols %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

aux <- cv.glmnet(XX, YY, family = "binomial",
                 keep = TRUE, nfolds=nrow(XX))
i <- which(aux$lambda == aux$lambda.min)
coefficients(aux, s = aux$lambda.min)

# Entender quais subjects o modelo erra mais
#erros = abs(aux$fit.preval[,i] - data_summary_cols$quiz_grade)
#dim(erros) = c(10, 16)
#colMeans(erros)

roc <- prediction(aux$fit.preval[,i], data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_glm <- tibble("espec" = roc@x.values[[1]],
                            "sens" = roc@y.values[[1]],
                            "method"   = "GLMNET")
data_roc_mean %<>% rbind(data_roc_mean_glm)


######################
# PCA + Regressão    #
######################

# Não deu certo. Ele joga todo mundo fora =(
ajuste_pca = data_summary_cols %>% 
  select(-patient, -video, -quiz_grade) %>%
  as.matrix() %>% 
  prcomp(center = FALSE)

XX = data_summary_cols %>% 
  model.matrix(quiz_grade~patient+video, .) %>% 
  cbind(ajuste_pca$x)
YY = data_summary_cols %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

aux <- cv.glmnet(XX, YY, family = "binomial",
                 keep = TRUE, nfolds=nrow(XX))
i <- which(aux$lambda == aux$lambda.min)
coefficients(aux, s = aux$lambda.min)

roc <- prediction(aux$fit.preval[,i], data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_glm <- tibble("espec" = roc@x.values[[1]],
                            "sens" = roc@y.values[[1]],
                            "method"   = "PCA-GLMNET")
data_roc_mean %<>% rbind(data_roc_mean_glm)


##########################
# Curvas ROC dos modelos #
##########################

gmean <- data_roc_mean %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = method),
            size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)
ggsave("./figuras/ROC_medias.pdf", plot = gmean)
gmean
