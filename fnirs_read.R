library(R.matlab)
library(tidyverse)
library(magrittr)

triggers = read.csv("triggers.csv")
triggers = triggers[, -1]
triggers = triggers[,-ncol(triggers)]
triggers = cbind(1, triggers)

# A partir da numeração de frames demarcada no trigger, a função marca qual 
# é o trecho de vídeo em questão (col_rel_frame)
# n_channel = número de canais existentes
# n_frames = número de frames no sinal de um paciente
# this_trigger = vetor com todos os triggers de um paciente
# 
generate_col_video = function(n_channel, n_frames, this_trigger)
{
  # número de trechos do sinal completo:
  n_videos = length(this_trigger) - 1 
  
  # vetor que indica o trecho do vídeo para cada frame:
  col_videos = NULL 
  
  # vetor que indica a posição do frame em relação ao trecho
  # do vídeo do qual faz parte:
  col_rel_frame = NULL 

# Para cada trecho do vídeo, calcula a quantidade de frames do vídeo
# subtraindo os triggers que dividem os trechos:
  
  for(ii in 1:n_videos) {
    # pega o comprimento de cada trecho:
    video_frames = as.numeric(this_trigger[ii+1] - this_trigger[ii])
    # acrescenta a col_videos um vetor com o número do trecho
    # repetido pelo comprimento do trecho (1,1,1,...,n,n,n,n,n) :
    col_videos = c(col_videos, rep(ii, video_frames))
    # acrescenta a col_rel_frame um vetor que varia de 1
    # até o comprimento do trecho:
    col_rel_frame = c(col_rel_frame, 1:video_frames)
  }
  
  # Para o último trecho do vídeo, contando os frames:
  video_frames = as.numeric(n_frames - this_trigger[n_videos+1] + 1)
  col_video = c(col_videos, rep(n_videos + 1, video_frames))
  col_rel_frame = c(col_rel_frame, 1:video_frames)
  
  # Organiza as colunas no data frame:
  tibble(video = rep(col_video, n_channel),
         rel_frame = rep(col_rel_frame, n_channel))
}

# Devolve um data frame com em que cada linha corresponde aos dados
# coletados em um frame de um canal de um paciente. 
# Os dados coletados são:
# número do canal, posição dos frames (relativa e absoluta), oxy e deoxy,
# trecho do vídeo, identificação do paciente, triggers, identificação do vídeo
clean_data = function(patient, signal) {
  n_channel = dim(signal)[2]
  n_frames = dim(signal)[1]
  col_patient = rep(patient, n_frames * n_channel)
  col_channel = rep(1:n_channel, each = n_frames)
  col_frame = rep(1:n_frames, n_channel)
  col_value = unlist(signal)
  this_trigger = triggers[patient,]
  video_data = generate_col_video(n_channel, n_frames, this_trigger)
  data = tibble(patient = col_patient,
                channel = col_channel,
                frame = col_frame,
                value = col_value,
                idx = 1:(n_frames*n_channel)
  )
  # Une as colunas:
  data = cbind(data, video_data)
  as.tibble(data)
}

# Calcula a quantidade de pacientes pela quantidade de arquivos Oxy 
num_patients = length(list.files("./Oxy/"))
patients = 1:num_patients

all_data = NULL

# Para cada paciente existente, transforma canal e sinais oxy e deoxy
# em forma estruturada de acordo com a função clean_data
for(patient in patients) {
  oxy_path = paste("./Oxy/", patient, ".txt", sep = "")
  clean_oxy = NULL
  if(file.exists(oxy_path)) {
    signal = read.table(oxy_path)
    clean_oxy = clean_data(patient, signal)
    clean_oxy %<>%  
      mutate(oxy = value) %>% 
      select(-value)
  }
  else {
    print("Arquivo não encontrado:")
    print(oxy_path)
  }
  
  clean_deoxy = NULL
  deoxy_path = paste("./Deoxy/", patient, ".txt", sep = "")
  if(file.exists(deoxy_path)) {
    signal = read.table(deoxy_path)
    clean_deoxy = clean_data(patient, signal)
    clean_deoxy %<>%  
      mutate(deoxy = value) %>% 
      select(-value)
  }
  else {
    print("Arquivo não encontrado:")
    print(deoxy_path)
  }

  # Caso de faltar informação de Oxy ou Deoxy de algum paciente.
  # Atribui NA
  patient_data = NULL
  join_vars = c("idx", "channel", "frame", "patient", "video", "rel_frame")
  if(!is.null(clean_oxy) && !is.null(clean_deoxy)) {
    patient_data = inner_join(clean_deoxy, clean_oxy, by = join_vars) 
  }
  else if(!is.null(clean_oxy))
  {
    patient_data = clean_oxy %>% 
      mutate(deoxy = NA)
  }
  else if(!is.null(clean_deoxy))
  {
    patient_data = clean_deoxy %>% 
      mutate(oxy = NA)
  }
  
  # Cola os dados de cada paciente:
  all_data = rbind(all_data, patient_data)
}

# Identifica quais sinais estão bons de acordo a matriz gerada no NirsLab:
good_channels = read.csv("Good_channels.csv")
good_channels = good_channels[, -1]

#Verifica qualidade do canal
clean_channel = function(patient, channel)
  good_channels[patient, channel]

# Pra cada paciente e cada canal, atribuo se o sinal está bom ou não
# para cada linha (frame, canal) verifica se está ok e mantém
# se o sinal for nulo ou ruim, marca como NA
# all_data é atualizada com os dados de oxy e deoxy substituídos por NA 
all_data %<>% 
  mutate(good_channel = map2_int(patient, channel, clean_channel),
         oxy = ifelse(good_channel, oxy, NA),
         deoxy = ifelse(good_channel, deoxy, NA)
  )

# Adiciona o resultado do quiz correspondente ao trecho do video:

# Identifica quais questões estão corretas para cada trecho de video:
quiz_grades = read.csv("quiz.csv")
quiz_grades = quiz_grades[, -1]

# Verifica se acertou a pergunta referente ao trecho do vídeo:
correct_quiz = function(patient, video)
{
  if(((video+1) %% 2) && video <= 20)
    return(quiz_grades[patient, as.integer(video/2)])
  return(NA)
} 
   
# Pra cada paciente e trecho do video, atribuo correto (1) ou incorreto (0)
# para cada linha (frame, video)
# all_data é atualizada com os resultados da questão na nova coluna
all_data %<>%
  mutate(quiz_grade = map2_int(patient, video, correct_quiz))

# Salva todo o data frame: 
write_rds(all_data, "clean_data.rds")
  