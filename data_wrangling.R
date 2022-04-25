library(tidyverse)
library(readxl)
library(janitor)
library(segregation)
library(censusapi)
library(tidycensus)

zip_tract <- read_excel("ZIP_TRACT_122021.xlsx")

# census api data

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
  regionin = "state:19+county:157",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

grinnell1 <- grinnell %>% 
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
  regionin = "state:51+county:680",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

liberty1 <- liberty %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

nyu <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:061",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

nyu1 <- nyu %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

oberlin <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:39+county:093",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

oberlin1 <- oberlin %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

pepperdine <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:06+county:037",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

pepperdine1 <- pepperdine %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

purdue <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:18+county:157",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

purdue1 <- purdue %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

rit <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:055",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

rit1 <- rit %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

rutgers <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:34+county:023",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

rutgers1 <- rutgers %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

smith <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:25+county:015",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

smith1 <- smith %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

soka <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:06+county:059",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

soka1 <- soka %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

swarthmore <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:42+county:045",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

swarthmore1 <- swarthmore %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

tcu <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:48+county:439",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

tcu1 <- tcu %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

juilliard <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:36+county:061",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

juilliard1 <- juilliard %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

thomasjeff <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:42+county:101",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

thomasjeff1 <- thomasjeff %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

arizona <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:04+county:019",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

arizona1 <- arizona %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

ucla <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:06+county:037",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

ucla1 <- ucla %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

ucsf <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:06+county:075",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

ucsf1 <- ucsf %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

uchicago <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:17+county:031",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

uchicago1 <- uchicago %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

delaware <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:10+county:003",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

delaware1 <- delaware %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

uiuc <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:17+county:019",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

uiuc1 <- uiuc %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

minntc <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:27+county:053",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

minntc1 <- minntc %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

tulsa <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:40+county:143",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

tulsa1 <- tulsa %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

utah <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:49+county:035",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

utah1 <- utah %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

washseattle <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:53+county:033",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

washseattle1 <- washseattle %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

virgpoly <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:51+county:121",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

virgpoly1 <- virgpoly %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

wakeforest <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:37+county:067",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

wakeforest1 <- wakeforest %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

washlee <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:51+county:678",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

washLEE <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:51+county:163",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

WASHlee <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:51+county:023",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

WASHLEE <- full_join(washlee,washLEE)

washlee1 <- full_join(WASHLEE,WASHlee) %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

washstate <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:53+county:075",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

washstate1 <- washstate %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

washu <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:29+county:189",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

washu1 <- washu %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

willmary <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:51+county:830",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

willmary1 <- willmary %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()

yale <- getCensus(
  name = "acs/acs5",
  vintage = 2019, 
  vars = c("B02001_002E","B02001_003E","B02001_004E","B02001_005E","B02001_006E", "B02001_007E"), 
  region = "tract:*", 
  regionin = "state:09+county:009",
  key = "7495b6f59b36203d746330a20f854cc8bfbf57b8",
)

yale1 <- yale %>%
  rename(white = 4, black = 5, indig = 6,
         asian = 7, pacific = 8, other = 9) %>% 
  gather(race, n, 4:9) %>% 
  mutual_total("race", "tract", weight = "n", se = TRUE) %>% 
  print()
