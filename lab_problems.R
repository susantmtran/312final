library(tidyverse)
library(readxl)
library(janitor)

uni_data <- read_excel("uni_data.xlsx") %>% clean_names()

### LAB 4

# problem 1 -- difference in means

summary(uni_data$end_endow)

low_endow <- uni_data %>% filter(end_endow <= median(uni_data$end_endow))
hi_endow <- uni_data %>% filter(end_endow >= median(uni_data$end_endow))

summary(low_endow$end_endow)
summary(hi_endow$end_endow)

t.test(hi_endow$end_endow, low_endow$end_endow, var.equal = FALSE)

summary(uni_data$h_index)

t.test(uni_data$h_index, mu = 0.5)
hist(uni_data$h_index)

# problem 2 -- correlation between variables

ggplot(uni_data, aes(end_endow, h_index)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "pink") +
  theme_light() +
  labs(x = "endowment at the end of fy19 ($)",
       y = "theil's information index")

ggplot(uni_data, aes(log(end_endow), h_index)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "pink") +
  theme_light() +
  labs(x = "log of endowment at the end of fy19 ($)",
       y = "theil's information index")

### LAB 5 

# problem 1 -- bivariate regression

lm_log <- lm(h_index ~ I(log(end_endow)), uni_data)

summary(lm_log)

plot(uni_data$h_index ~ I(log(uni_data$end_endow)),
     xlab = "log of market value of endowment ($)",
     ylab = "theil's information index")

abline(lm_log)

### LAB 6

# problem 1 -- multivariate regression

uni_data$ranking <- as.numeric(uni_data$ranking)

model <- lm(h_index ~ I(log(end_endow)) + factor(ranking) + year, uni_data)
summary(model)

model2 <- lm(h_index ~ I(log(end_endow)) + factor(ranking):year, uni_data)
summary(model2)

library(stargazer)
stargazer(lm_log, model, type="text", out="reg_table.html")
