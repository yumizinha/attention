library(tidyverse)

all_data = read_rds("clean_data.rds")

summary = all_data %>%
  filter((video %% 2) == 0,
         video != 22) %>% 
  group_by(rel_frame, video, channel) %>%
  summarise(oxy_m = mean(oxy, na.rm = TRUE),
            oxy_sd = sd(oxy, na.rm = TRUE),
            deoxy_m = mean(deoxy, na.rm = TRUE),
            deoxy_sd = sd(deoxy, na.rm = TRUE)
  )

summary %>% 
  ggplot(aes(x = rel_frame, y = oxy_m)) +
  geom_line() + 
  facet_grid(channel ~ video)
ggsave("oxy_m.pdf")

summary %>% 
  ggplot(aes(x = rel_frame, y = oxy_m)) +
  geom_line() + 
  facet_grid(channel ~ video)
ggsave("deoxy_m.pdf")

