library(tidyverse)

uni_data <- read_excel("uni_data.xlsx") %>% clean_names()

ggplot(uni_data, aes(end_endow)) +
  geom_histogram(aes(y = ..density..), bins = 50) +
  stat_function(fun = dnorm,
                args = list(mean = mean(uni_data$end_endow),
                            sd = sd(uni_data$end_endow)),
                color = "pink",
                size = 1) +
  theme_light() +
  labs(x = "endowment at the end of fy19 ($)",
       y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggplot(uni_data, aes(m_index)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(uni_data$m_index),
                            sd = sd(uni_data$m_index)),
                color = "pink",
                size = 1) +
  theme_light() +
  labs(x = "mutual information index",
       y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggplot(uni_data, aes(h_index)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(uni_data$h_index),
                            sd = sd(uni_data$h_index)),
                color = "pink",
                size = 1) +
  theme_light() +
  labs(x = "theil's information index",
       y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggplot(uni_data, aes(end_endow, h_index)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme_light() +
  labs(x = "endowment at the end of fy19 ($)",
       y = "theil's information index")
