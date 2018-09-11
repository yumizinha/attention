library(tidyverse)

all_data = read_rds("clean_data.rds")

summary = all_data %>%
  filter((video %% 2) == 0,
         video != 22) %>% 
  group_by(rel_frame, video, channel) %>%
  summarise(oxy_m = mean(oxy, na.rm = TRUE),
            deoxy_m = mean(deoxy, na.rm = TRUE)
  )

# Gerando os gráficos de oxy:
for(ii in 1:10)
{
  for(jj in 1:2)
  {
    all_data %>%
      filter(video %% 2 == 0,
             video <= jj*10,
             video > (jj-1)*10,
             channel == ii,
             !is.na(oxy)) %>% 
      ggplot(aes(x = rel_frame, y = oxy)) +
      geom_line(aes(color = as.factor(patient))) +
      geom_smooth() +
      facet_wrap(~ video, ncol = 2)
    ggsave(paste("./figuras/oxy_m-",
                 ii, "-", jj, ".pdf", sep=""))
  }
}

# Gerando os gráficos de deoxy:
for(ii in 1:10)
{
  for(jj in 1:2)
  {
    all_data %>%
      filter(video %% 2 == 0,
             video <= jj*10,
             video > (jj-1)*10,
             channel == ii,
             !is.na(deoxy)) %>% 
      ggplot(aes(x = rel_frame, y = deoxy)) +
      geom_line(aes(color = as.factor(patient))) +
      geom_smooth() +
      facet_wrap(~ video, ncol = 2)
    ggsave(paste("./figuras/deoxy_m-",
                 ii, "-", jj, ".pdf", sep=""))
  }
}

summary %>% 
  ggplot(aes(x = rel_frame, y = oxy_m)) +
  geom_line() + 
  facet_grid(channel ~ video)
ggsave("deoxy_m_1.pdf")

