library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(plotly)

###############################
# Pre-processamento dos dados #
###############################


#all_data = read_rds("clean_data.rds")
all_data = read_rds("clean_interpol_data.rds")

data_summary = all_data %>%
  # Retirar ids: 4, 9, 13 e 14. Pior mesmo era o 14
  # filter(!(patient %in% c(4, 9, 13, 14))) %>%
  filter(!(patient %in% c(14))) %>%
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

colnames(data_summary_cols)

##############################
# ANOVA                      #
##############################

summary(lm(oxy_m ~ quiz_grade + patient + channel, data = data_summary))
summary(lm(deoxy_m ~ quiz_grade + patient + channel, data = data_summary))
# montar mapa por canal pra ver onde o p-valor dá menor que 0.05/20



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
  select(-video) %>% 
  mutate(quiz_grade = as.numeric(quiz_grade)) %>% 
  randomForest(formula, .)

roc <- prediction(ajuste$predicted, data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_rf <- tibble("espec" = roc@x.values[[1]],
                           "sens" = roc@y.values[[1]],
                           "method"   = "RF")
data_roc_mean %<>% rbind(data_roc_mean_rf)

# Analisando os canais da saida de RF
imp = importance(ajuste)[,1]
imp = sort(imp[imp<4])
p  <- plot_ly(
  x = names(imp), y = imp,
  type = "bar",
  name = "RF"
)
p

######################
# Regressao Logistica #
######################

data_summary_cols %<>% arrange(patient)

# pega o fold para cada patient:
foldid_p = 1 + ((order(data_summary_cols$patient) - 1) %/% 10)
nfolds_p = max(foldid_p)

colnames(data_summary_cols)
fm = quiz_grade~.
XX= data_summary_cols %>% select(-video) %>% model.matrix(fm, .)

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
data_roc_mean_glm <- tibble("espec" = roc@x.values[[1]],
                            "sens" = roc@y.values[[1]],
                            "method"   = "GLMNET")
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



#############################
# Teste Permutação - GLMNET #
#############################

# rodar umas mil vezes essa ROC dos dados permutados.
# ver qual proporção deu maior ou igual a ROC da original.
# pego o p-valor dessa distribuição

# inicialização do arquivo
# permutacoes = NULL
# write_rds(permutacoes, "permutacoes_auc.rds")

for(n in 1:1000){
  
  data_permutado = data_summary_cols
  data_permutado$quiz_grade = sample(data_permutado$quiz_grade)
  
  XX = model.matrix(quiz_grade~., data_permutado)
  YY = data_permutado %>% ungroup() %>% select(quiz_grade) %>% as.matrix()
  
  aux <- cv.glmnet(XX, YY, family = "binomial",
                   keep = TRUE, nfolds = nfolds_p,
                   foldid = foldid_p)
  i <- which(aux$lambda == aux$lambda.min)
  coefficients(aux, s = aux$lambda.min)
  
  # Entender quais subjects o modelo erra mais
  #erros = abs(aux$fit.preval[,i] - data_permutado$quiz_grade)
  #dim(erros) = c(10, 16)
  #colMeans(erros)
  
  roc <- prediction(aux$fit.preval[,i], data_permutado$quiz_grade)
  permutacoes = read_rds("permutacoes_auc.rds")
  permutacoes = c(permutacoes, 
                  performance(roc, measure = "auc")@y.values[[1]])
  print(n, performance(roc, measure = "auc")@y.values[[1]])
  write_rds(permutacoes, "permutacoes_auc.rds")
}

# write_rds(permutacoes_2, "permutacoes_auc.rds")
verificacao = read_rds("permutacoes_auc.rds")
maior = verificacao
maior[which(verificacao >= 0.695613)]
length(verificacao)

# Pegar os principais canais e montar o mapa dos principais.




#############################
# Teste Permutação - RPART #
#############################

# write_rds(NULL, "permutacoes_auc_rpart.rds")


for(n in 1:1000){
  data_permutado_rpart = data_summary_cols
  data_permutado_rpart$quiz_grade = sample(data_permutado_rpart$quiz_grade)
  
  ## Arvore com todo mundo
  # Ajuste da arvore. Acertos em função de oxy_m_1: 
  formula = quiz_grade~.
  ajuste_todos = data_permutado_rpart %>% 
    select(-patient, -video) %>% 
    rpart(formula, .)
  
  # Identificando o nível da árvore com melhor erro de validação cruzada:
  melhor_nivel = which.min(ajuste_todos$cptable[,"xerror"])
  
  # Identifica cp associado ao melhor nivel:
  cp = ajuste_todos$cptable[melhor_nivel, "CP"]
  
  ajuste_todos %<>% prune(cp) 
  roc <- prediction(predict(ajuste_todos),
                    data_permutado_rpart$quiz_grade)
  performance(roc, measure = "auc")
  performance(roc, measure = "auc")@y.values[[1]]
  
  #write_rds(permutacoes_rpart, "permutacoes_auc_rpart.rds")
  
  permutacoes_rpart = read_rds("permutacoes_auc_rpart.rds")
  permutacoes_rpart = c(permutacoes_rpart, 
                        performance(roc, measure = "auc")@y.values[[1]])
  print(n, performance(roc, measure = "auc")@y.values[[1]])
  write_rds(permutacoes_rpart, "permutacoes_auc_rpart.rds")
  
  
  roc <- performance(roc, measure = "tpr", x.measure = "fpr")
  data_roc_mean_rpart <- tibble("espec" = roc@x.values[[1]],
                                "sens" = roc@y.values[[1]],
                                "method"   = "RPART")
  data_roc_mean %<>% rbind(data_roc_mean_rpart)
  
}  

verificacao = read_rds("permutacoes_auc_rpart.rds")
maior = verificacao
maior[which(verificacao >= 0.50)]
length(maior[which(verificacao >= 0.5)])
length(verificacao)
p_value = length(maior[which(verificacao >= 0.5)]) / length(verificacao)


# write_rds(permutacoes_rf, "permutacoes_auc_rf.rds")


####################################
# Teste Permutação - Random Forest #
####################################

# write_rds(NULL, "permutacoes_auc_rf.rds")
for(n in 1:1000){
  
  data_permutado_rf = data_summary_cols
  data_permutado_rf$quiz_grade = sample(data_permutado_rf$quiz_grade)
  
  
  formula = quiz_grade~. 
  
  # Classificação usa fatores, regressão usa números reais, por isso
  # colocar em fator:
  ajuste = data_permutado_rf %>%
    mutate(quiz_grade = as.numeric(quiz_grade)) %>% 
    randomForest(formula, .)
  
  roc <- prediction(ajuste$predicted, data_permutado_rf$quiz_grade)
  performance(roc, measure = "auc")
  
  performance(roc, measure = "auc")@y.values[[1]]
  
  permutacoes_rf = read_rds("permutacoes_auc_rf.rds")
  
  permutacoes_rf = c(permutacoes_rf, 
                     performance(roc, measure = "auc")@y.values[[1]])
  print(n, performance(roc, measure = "auc")@y.values[[1]])
  write_rds(permutacoes_rf, "permutacoes_auc_rf.rds")
  
  
  roc <- performance(roc, measure = "tpr", x.measure = "fpr")
  data_roc_mean_rf <- tibble("espec" = roc@x.values[[1]],
                             "sens" = roc@y.values[[1]],
                             "method"   = "RF")
  data_roc_mean %<>% rbind(data_roc_mean_rf)
}


verificacao = read_rds("permutacoes_auc_rf.rds")
maior = verificacao
maior[which(verificacao >= 0.6819411)]
length(maior[which(verificacao >= 0.6819411)])
length(verificacao)
p_value = length(maior[which(verificacao >= 0.6819411)]) / length(verificacao)
p_value


# PARA DOCUMENTAÇÃO DA DISSERTAÇÃO
# Interpretação da hemodinâmica varia conforme cada vídeo: 
# retirada do video interfere na regressão e na RF
# informação de oxy e deoxy melhora o modelo

