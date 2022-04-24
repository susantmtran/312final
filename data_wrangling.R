# 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

library(tidyverse)
library(janitor)
library(segregation)
library(censusapi)

zip_tract <- read_excel("ZIP_TRACT_122021.xlsx")

# census api data
# 7495b6f59b36203d746330a20f854cc8bfbf57b8

# figuring out how to get the data with northwestern first

northwestern <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:17+county:031",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
  )

northwestern <- northwestern %>%
  rename(white = 4,
         black = 5,
         indig = 6,
         asian = 7,
         pacific = 8,
         other = 9)

# long format as required for the segregation package
northwestern <- gather(northwestern, race, n, 4:9)

northwestern1 <- mutual_total(northwestern, "race", "tract", weight = "n", se = TRUE)

# all other schools

baylor <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:48+county:309",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

baylor1 <- baylor %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE)

baylor <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:48+county:309",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

baylor1 <- baylor %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE)


# compiling university data

uni_data <- read_csv("uni_data.csv") %>%
  rename(institution = 2,
         beg_endow = 3,
         end_endow = 4,
         fte = 5,
         city = 6,
         state = 7,
         zip_code = 8,
         county = 9,
         cbsa = 10)
