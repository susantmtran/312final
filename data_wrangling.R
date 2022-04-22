library(tidyverse)
library(janitor)
library(segregation)

zip_tract <- read_excel("zip_tract_062019.xlsx")

zip_tract %>% filter(zip == c(
  "76798","30149","02467","02215","02454")
)

uni_data <- read_csv("uni_data.csv") %>% clean_names()
uni_data

uni_data <- uni_data %>% rename(institution = 2,
                                beg_endow = 3,
                                end_endow = 4,
                                fte = 5,
                                city = 6,
                                state = 7,
                                zip_code = 8,
                                county = 9,
                                cbsa = 10)

nu_test_data <- read_excel("nu_test_data.xlsx", sheet = "Data", skip = 2)
nu_test_data

nu_test_data <- gather(nu_test_data, tract, value, 2:7)
nu_test_data$value <- as.numeric(gsub(",","",nu_test_data$value))

mutual_total(nu_test_data, "tract", "grouping", weight = "value")
