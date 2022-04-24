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

berry <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:13+county:115",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

berry1 <- berry %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE)

bostonc <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:25+county:017",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

bostonc1 <- bostonc %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  view()

bostonu <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:25+county:021",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

bostonu1 <- bostonu %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  view()

brandeis <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:25+county:017",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

brandeis1 <- brandeis %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  view()

brown <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:44+county:007",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

brown1 <- brown %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  view()

brynmawr <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:42+county:091",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

brynmawr1 <- brynmawr %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  view()

carleton <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:27+county:049",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

carleton1 <- carleton %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

carnegie <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:42+county:003",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

carnegie1 <- carnegie %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

colgate <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:053",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

colgate1 <- colgate %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

columbia <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:061",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

columbia1 <- columbia %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

duke <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:37+county:063",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

duke1 <- duke %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

georgewash <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:11+county:001",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

georgewash1 <- georgewash %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

grinnell <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:19+county:099",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

GRINNELL <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:19+county:157",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

grinnell1 <- full_join(grinnell,GRINNELL) %>% 
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

hamilton <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:065",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

hamilton1 <- hamilton %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

harvard <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:25+county:017",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

harvard1 <- harvard %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

ilwesley <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:17+county:113",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

ilwesley1 <- ilwesley %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

iowastate <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:19+county:169",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

iowastate1 <- iowastate %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

johnshopkins <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:24+county:510",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

johnshopkins1 <- johnshopkins %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

liberty <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:24+county:510",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

liberty1 <- liberty %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

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
