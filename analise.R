library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(plotly)
library(e1071)
library(caret)
library(gmodels)


###############################
# Pre-processamento dos dados #
###############################


#all_data = read_rds("clean_data.rds")
all_data = read_rds("clean_interpol_data.rds")


# Tirar a média do sinal para eliminação de baseline dos sujeitos
all_data %<>% 
  group_by(patient, channel) %>%
  summarise(oxy_m = mean(oxy, na.rm = TRUE),
            deoxy_m = mean(deoxy, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  inner_join(all_data, ., by = c("patient", "channel")) %>% 
  mutate(oxy = oxy - oxy_m, 
         deoxy = deoxy - deoxy_m)


# Dados extraídos da série temporal
data_summary = all_data %>%
  # Retirar ids: se der 4, 9, 13 e 14. Pior mesmo era o 14
  filter(!(patient %in% c(14))) %>%
  filter(!is.na(oxy) & !is.na(deoxy)) %>%
  group_by(patient, video, channel) %>%
  summarise(
            #oxy_m = mean(oxy, na.rm = TRUE),
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
    select(patient, 
           video, 
           #oxy_m,
           deoxy_m
    )
  # [3:4] para oxy + deoxy, [3:3] pra uma só
  colnames(new_data)[3:3] = paste(colnames(new_data)[3:3],
                                  ii, 
                                  sep = "_")
  data_summary_cols %<>% left_join(new_data,
                                   by = c("patient", "video")) 
}

#write_rds(data_summary_cols, "data_summary_cols.rds")

data_summary_cols = read_rds("data_summary_cols.rds") 

colnames(data_summary_cols)


##############################
# ANOVA                      #
##############################

# Delineamento do quanto a oxy/deoxy é explicada 
# pelos acertos e erros. Testando se Oxy e deoxy 
# de quem acerta é diferente de quem erra

# montar mapa por canal pra ver onde o p-valor dá menor que 0.05/20
summary(lm(oxy_m_18 ~ quiz_grade + patient, data = data_summary_cols))
summary(lm(deoxy_m_18 ~ quiz_grade + patient, data = data_summary_cols))


##############################
# Modelinhos                 #
##############################
data_roc_mean = tibble()


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
result = NULL
result_type = NULL
patients = unique(data_summary_cols$patient)
for(patient_i in patients){
  data_summary_cols_cv = data_summary_cols %>% 
    filter(patient != patient_i)
  ajuste = data_summary_cols_cv %>%
    mutate(quiz_grade = as.numeric(quiz_grade)) %>% 
    randomForest(formula, .)
  data_summary_cols_test = data_summary_cols %>% 
    filter(patient == patient_i)
  result = c(result,
             predict(ajuste, data_summary_cols_test))
  result_type = c(result_type,
                  predict(ajuste, 
                          data_summary_cols_test,
                          type = "response"))
}

# Matriz de prob pra salvar e fazer boxplot depois
# write_excel_csv(as.data.frame(result_type), "RF_prob.csv")



# A partir da espec = sensbilidade, determina corte de acertos
result_class = as.factor(result_type > 0.705)
reference = as.factor(data_summary_cols$quiz_grade > 0.705)

# Matriz com previsões de acertos e erros para cada pessoa e questão
# a partir do corte sens = espec
result_rf_class = matrix(NA, nrow = 18, ncol = 10)
for(ii in 1:18)
{
  for(jj in 1:10)
  {
    result_rf_class[ii, jj] = result_class[10*(ii-1) + jj]
  }
}
# gambi pra transformar os dados em 0 e 1 
# (problemas R-ísticos de factor em matriz)
result_rf_class = result_rf_class-1

# write_excel_csv(as.data.frame(result_rf_class), "RF_predicao.csv")
aux_true = result_class[which(reference == "TRUE")] == "TRUE"
B = 10^4
boot_sens = rep(NA, B)
for(ii in 1:B) boot_sens[ii] = mean(sample(aux_true, length(aux_true), replace = TRUE))
mean(boot_sens)
sd(boot_sens)

aux_false = result_class[which(reference == "FALSE")] == "FALSE"
B = 10^4
boot_spec = rep(NA, B)
for(ii in 1:B) boot_spec[ii] = mean(sample(aux_false, length(aux_false), replace = TRUE))
mean(boot_spec)
sd(boot_spec)

confusionMatrix(result_class, reference, dnn = c("result", "quiz_grade"))

roc <- prediction(result, data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_rf <- tibble("espec" = roc@x.values[[1]],
                           "sens" = roc@y.values[[1]],
                           "method"   = "RF")
data_roc_mean %<>% rbind(data_roc_mean_rf)

# Entender quais subjects o modelo erra mais
erros = abs(ajuste$predicted - data_summary_cols$quiz_grade)
dim(erros) = c(10, 18)
colMeans(erros) # erros preditos dos pacientes: piores sao 4, 9 e 15
rowMeans(erros) # questoes que o modelo é pior: 2, 3, 7 e 8


# Analisando os canais da saida de RF
imp = importance(ajuste)[,1]
imp = sort(imp[imp<4])
p  <- plot_ly(
  x = names(imp), y = imp,
  type = "bar",
  name = "RF"
  )
p

#######################
# Regressao Logistica #
#######################
data_summary_cols %<>% arrange(patient)
fm = quiz_grade ~. 

result = NULL
result_type_glmnet = NULL
patients = unique(data_summary_cols$patient)
for(patient_i in patients){
  
  data_summary_cols_cv = data_summary_cols %>%
    filter(patient != patient_i)
  # pega o fold para cada patient:
  foldid_p = 1 + ((order(data_summary_cols_cv$patient) - 1) %/% 10)
  nfolds_p = max(foldid_p)
  
  XX = data_summary_cols_cv %>% 
    model.matrix(fm, .)
  
  YY = data_summary_cols_cv %>% 
    ungroup() %>% 
    select(quiz_grade) %>% 
    as.matrix()
  
  aux <- cv.glmnet(XX, YY, family = "binomial",
                   keep = TRUE, nfolds = nfolds_p,
                   foldid = foldid_p)
  # i <- which(aux$lambda == aux$lambda.min)

  data_summary_cols_test = data_summary_cols %>%
    filter(patient == patient_i)
  
  XX = data_summary_cols_test %>% 
    model.matrix(fm, .)
  
  YY = data_summary_cols_test %>% 
    ungroup() %>% 
    select(quiz_grade) %>% 
    as.matrix()
  
  result = c(result, predict(aux, XX, type = "response"))
 # result_type_glmnet = c(result_type_glmnet,
#                  predict(aux, 
 #                         data_summary_cols_test,
  #                        type = "response"))
  
  }

# gambi mágica pro plot funcionar
# exibição do gráfico dos resultados de AUC 
# e valores de λ mínimo (para AUC mínimo)
# e 1se (para AUC menor um desvio padrão do mínimo)
#graphics.off()
#par("mar")
#par(mar=c(1,1,1,1))

#plot(aux, xlab="Mean-squared error", ylab="log labda")
#mycoefs <- coef(aux)@Dimnames[[1]][-1]
#cbind(1:length(mycoefs), mycoefs)

# op <- par(mfrow=c(1, 2))
# plot(aux$glmnet.fit, xvar="norm", label=TRUE, 
#      xlab="L1 norm", ylab="Coefficients")
# plot(aux$glmnet.fit, xvar="lambda", label=TRUE,
#      xlab="log lambda", ylab="Coefficients")
# par(op)
# 
# aux$lambda.min
# aux$lambda.1se

# Entender quais subjects o modelo erra mais
# erros = abs(aux$fit.preval[,i] - data_summary_cols$quiz_grade)
# dim(erros) = c(10, 18)
# colMeans(erros) # erros preditos dos pacientes: piores sao 4, 9 e 13
# rowMeans(erros) # questoes que o modelo é pior: 2, 3, 7, 8

# Matriz de prob pra salvar e fazer boxplot depois
# write_excel_csv(as.data.frame(result), "GLMNET_prob.csv")


limiar = 0.7225
result_class = as.factor(result > limiar)
reference = as.factor(data_summary_cols$quiz_grade > limiar)

aux_true = result_class[which(reference == "TRUE")] == "TRUE"
B = 10^4
boot_sens = rep(NA, B)
for(ii in 1:B) boot_sens[ii] = mean(sample(aux_true, length(aux_true), replace = TRUE))
mean(boot_sens)
sd(boot_sens)

aux_false = result_class[which(reference == "FALSE")] == "FALSE"
B = 10^4
boot_spec = rep(NA, B)
for(ii in 1:B) boot_spec[ii] = mean(sample(aux_false, length(aux_false), replace = TRUE))
mean(boot_spec)
sd(boot_spec)

confusionMatrix(result_class, reference, dnn = c("result", "quiz_grade"))


# Matriz com previsões de acertos e erros para cada pessoa e questão
# a partir do corte sens = espec
result_glmnet_class = matrix(NA, nrow = 18, ncol = 10)
for(ii in 1:18)
{
  for(jj in 1:10)
  {
    result_glmnet_class[ii, jj] = result_class[10*(ii-1) + jj]
  }
}
# gambi pra transformar os dados em 0 e 1 
# (problemas R-ísticos de factor em matriz)
result_glmnet_class = result_glmnet_class-1

# write_excel_csv(as.data.frame(result_glmnet_class), "GLMNET_predicao.csv")




roc <- prediction(result, data_summary_cols$quiz_grade)
performance(roc, measure = "auc")
roc <- performance(roc, measure = "tpr", x.measure = "fpr")
data_roc_mean_glm <- tibble("espec" = roc@x.values[[1]],
                            "sens" = roc@y.values[[1]],
                            "method"   = "GLMNET")
data_roc_mean %<>% rbind(data_roc_mean_glm)



######################
# PCA + Regressão    #
######################

ajuste_pca = data_summary_cols %>% 
  select(-patient, -video, -quiz_grade) %>%
  as.matrix() %>% 
  prcomp(center = FALSE)

d=3
data_summary_cols_pca = data_summary_cols %>% 
  select(patient, quiz_grade, video) %>%
  cbind(ajuste_pca$x[,1:d]) %>% 
  as_tibble()

result = NULL
patients = unique(data_summary_cols_pca$patient)
for(patient_i in patients){
  
  data_summary_cols_cv = data_summary_cols_pca %>%
    filter(patient != patient_i)
  # pega o fold para cada patient:
  foldid_p = 1 + ((order(data_summary_cols_cv$patient) - 1) %/% 10)
  nfolds_p = max(foldid_p)
  
  XX = data_summary_cols_cv %>% 
    model.matrix(fm, .)
  YY = data_summary_cols_cv %>% 
    ungroup() %>% 
    select(quiz_grade) %>% 
    as.matrix()

  aux <- cv.glmnet(XX, YY, family = "binomial",
                   keep = TRUE, nfolds = nfolds_p,
                   foldid = foldid_p)
  # i <- which(aux$lambda == aux$lambda.min)
  
  data_summary_cols_test = data_summary_cols_pca %>%
    filter(patient == patient_i)
  
  XX = data_summary_cols_test %>% 
    model.matrix(fm, .)
  
  YY = data_summary_cols_test %>% 
    ungroup() %>% 
    select(quiz_grade) %>% 
    as.matrix()
  
  result = c(result, predict(aux, XX, type = "response"))
}


roc <- prediction(result, data_summary_cols_pca$quiz_grade)
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



#############################
# Teste Permutação - GLMNET #
#############################

# write_rds(NULL, "permutacoes_auc_glmnet.rds")

# rodar umas mil vezes essa ROC dos dados permutados.
# ver qual proporção deu maior ou igual a ROC da original.
# pego o p-valor dessa distribuição

# inicialização do arquivo
# permutacoes = NULL
# write_rds(permutacoes, "permutacoes_auc.rds")

for(n in 1:1000){
  data_permutado = data_summary_cols
  data_permutado$quiz_grade = sample(data_permutado$quiz_grade)
  
  
  data_permutado %<>% arrange(patient)
  fm = quiz_grade~.
  
  result = NULL
  patients = unique(data_permutado$patient)
  for(patient_i in patients){
    
    data_summary_cols_cv = data_permutado %>%
      filter(patient != patient_i)
    # pega o fold para cada patient:
    foldid_p = 1 + ((order(data_summary_cols_cv$patient) - 1) %/% 10)
    nfolds_p = max(foldid_p)
    
    XX = data_summary_cols_cv %>% 
      model.matrix(fm, .)
    
    YY = data_summary_cols_cv %>% 
      ungroup() %>% 
      select(quiz_grade) %>% 
      as.matrix()
    
    aux <- cv.glmnet(XX, YY, family = "binomial",
                     keep = TRUE, nfolds = nfolds_p,
                     foldid = foldid_p)
    # i <- which(aux$lambda == aux$lambda.min)
    
    data_summary_cols_test = data_permutado %>%
      filter(patient == patient_i)
    
    XX = data_summary_cols_test %>% 
      model.matrix(fm, .)
    
    YY = data_summary_cols_test %>% 
      ungroup() %>% 
      select(quiz_grade) %>% 
      as.matrix()
    
    result = c(result, predict(aux, XX, type = "response"))
  }
  
  roc <- prediction(result, data_permutado$quiz_grade)
  permutacoes = read_rds("permutacoes_auc_glmnet.rds")
  permutacoes = c(permutacoes, 
                  performance(roc, measure = "auc")@y.values[[1]])
  print(n, performance(roc, measure = "auc")@y.values[[1]])
  write_rds(permutacoes, "permutacoes_auc_glmnet.rds")
  
  performance(roc, measure = "auc")
  roc <- performance(roc, measure = "tpr", x.measure = "fpr")
  data_roc_mean_glm <- tibble("espec" = roc@x.values[[1]],
                              "sens" = roc@y.values[[1]],
                              "method"   = "GLMNET")
  data_roc_mean %<>% rbind(data_roc_mean_glm)
  
  
  
    
  # data_permutado = data_summary_cols
  # data_permutado$quiz_grade = sample(data_permutado$quiz_grade)
  # 
  # XX = model.matrix(quiz_grade~., data_permutado)
  # YY = data_permutado %>% ungroup() %>% select(quiz_grade) %>% as.matrix()
  # 
  # aux <- cv.glmnet(XX, YY, family = "binomial",
  #                  keep = TRUE, nfolds = nfolds_p,
  #                  foldid = foldid_p)
  # i <- which(aux$lambda == aux$lambda.min)
  # coefficients(aux, s = aux$lambda.min)
  # 
  #   
  # roc <- prediction(aux$fit.preval[,i], data_permutado$quiz_grade)
  # permutacoes = read_rds("permutacoes_auc_glmnet.rds")
  # permutacoes = c(permutacoes, 
  #                 performance(roc, measure = "auc")@y.values[[1]])
  # print(n, performance(roc, measure = "auc")@y.values[[1]])
  # write_rds(permutacoes, "permutacoes_auc_glmnet.rds")
}

# write_rds(permutacoes_2, "permutacoes_auc_glmnet.rds")
verificacao = read_rds("permutacoes_auc_glmnet.rds")
maior = verificacao
maior[which(verificacao >= 0.6340144)]
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


####################################
# Teste Permutação - Random Forest #
####################################

# write_rds(NULL, "permutacoes_auc_xx_rf.rds")
# Já foi feito 90 simulações.

for(n in 1:1000){
  
  data_permutado_rf = data_summary_cols
  data_permutado_rf$quiz_grade = sample(data_permutado_rf$quiz_grade)
  
  formula = quiz_grade~. 
  
  result = NULL
  patients = unique(data_permutado_rf$patient)
  for(patient_i in patients){
    data_summary_cols_cv = data_permutado_rf %>% 
      filter(patient != patient_i)
    ajuste = data_summary_cols_cv %>%
      mutate(quiz_grade = as.numeric(quiz_grade)) %>% 
      randomForest(formula, .)
    data_summary_cols_test = data_permutado_rf %>% 
      filter(patient == patient_i)
    result = c(result,
               predict(ajuste, data_summary_cols_test))
  }
  
  roc <- prediction(result, data_permutado_rf$quiz_grade)
  performance(roc, measure = "auc")
  
  performance(roc, measure = "auc")@y.values[[1]]
  
  permutacoes_rf = read_rds("permutacoes_auc_xx_rf.rds")
  permutacoes_rf = c(permutacoes_rf,
                     performance(roc, measure = "auc")@y.values[[1]])
  print(n, performance(roc, measure = "auc")@y.values[[1]])
  write_rds(permutacoes_rf, "permutacoes_auc_xx_rf.rds")
  
  roc <- performance(roc, measure = "tpr", x.measure = "fpr")
  data_roc_mean_rf <- tibble("espec" = roc@x.values[[1]],
                             "sens" = roc@y.values[[1]],
                             "method"   = "RF")
  data_roc_mean %<>% rbind(data_roc_mean_rf)
}


verificacao = read_rds("permutacoes_auc_xx_rf.rds")
maior = verificacao
maior[which(verificacao >= 0.654)]
length(maior[which(verificacao >= 0.654)])
length(verificacao)
p_value = length(maior[which(verificacao >= 0.654)]) / length(verificacao)
p_value


#######################################
# Comparacao expectativas vs modelo   #
#######################################

perc_tab = read_csv("percepcao_voluntario.csv")
glm_tab = read_csv("GLMNET_predicao.csv")
glm_prob_tab = read_csv("GLMNET_prob_asmatrix.csv")
rf_tab = read_csv("RF_predicao.csv")
rf_prob_tab = read_csv("RF_prob_asmatrix.csv")

glm_df = glm_tab %>% 
  as.data.frame() %>% 
  gather("questao", "pred_glm") %>% 
  mutate(ind = rep(1:18, 10),
         questao = gsub("V", "", questao))

glm_prob_df = glm_prob_tab %>% 
  as.data.frame() %>% 
  gather("questao", "prob_glm") %>% 
  mutate(ind = rep(1:18, 10),
         questao = gsub("V", "", questao))

rf_df = rf_tab %>% 
  as.data.frame() %>% 
  gather("questao", "pred_rf") %>% 
  mutate(ind = rep(1:18, 10),
         questao = gsub("V", "", questao))

rf_prob_df = rf_prob_tab %>% 
  as.data.frame() %>% 
  gather("questao", "prob_rf") %>% 
  mutate(ind = rep(1:18, 10),
         questao = gsub("V", "", questao))
rf_prob_df$prob_rf[rf_prob_df$prob_rf > 1] =
  rf_prob_df$prob_rf[rf_prob_df$prob_rf > 1]/1000

perc_df = perc_tab[1:18,-c(1:2)] %>% 
  as.data.frame() %>% 
  gather("questao", "perc") %>% 
  mutate(ind = rep(1:18, 10))

full_df = glm_df %>% 
  inner_join(rf_df) %>% 
  inner_join(perc_df)

CrossTable(full_df$pred_rf, full_df$perc)
CrossTable(full_df$pred_glm, full_df$perc)

inner_join(rf_prob_df, perc_df) %>% 
  mutate(perc = as.factor(perc)) %>% 
  ggplot(aes(x = perc, y = prob_rf)) +
  geom_boxplot()

inner_join(glm_prob_df, perc_df) %>% 
  mutate(perc = as.factor(perc)) %>% 
  ggplot(aes(x = perc, y = prob_glm)) +
  geom_boxplot()


# Comparando os grupos RF
inner_join(rf_prob_df, perc_df) %>% 
  filter(perc != 0.5) %>% 
  mutate(perc = as.factor(perc)) %>%
  lm(prob_rf ~ perc, data = .) %>% 
  summary()

# Comparando os grupos GLMNET
inner_join(glm_prob_df, perc_df) %>% 
  #filter(perc != 0.5) %>% 
  mutate(perc = as.factor(perc)) %>%
  lm(prob_glm ~ perc, data = .) %>% 
  summary()



#########################
# SVM nunca terminado   #
#########################

# Implementação desse treco:
# 1) Ver se faz sentido
# 2) Implementar

data_summary_cols %<>% arrange(patient)

colnames(data_summary_cols)
fm = quiz_grade~.
XX= data_summary_cols %>% model.matrix(fm, .)
YY = data_summary_cols %>% ungroup() %>% select(quiz_grade) %>% as.matrix()

# Criando modelo SVM a partir do conjunto de dados iris
ajuste = XX
  aux <- svm(quiz_grade~., data = XX)

# Resumo do modelo
summary(aux)
