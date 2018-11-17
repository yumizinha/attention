library(glmnet)
library(ROCR)
library(stats)
library(tidyverse)
library(magrittr)
library(rpart)
library(rpart.plot)

# Arvore da Yuyu

dados = tibble(X1 = rbinom(1000, 1, 0.5),
               X2 = rbinom(1000, 1, 0.5))

dados %<>%
  mutate(Y = (X1 | X2) & !(X1 & X2)) %>%
  mutate(Y = ifelse(rbinom(1000, 1, 0.95), Y, 1-Y))

# ponto pra chamar a entrada do %>% (dados)
dados %>%
  rpart(Y ~ X1 + X2, .) %>% 
  rpart.plot()
        
