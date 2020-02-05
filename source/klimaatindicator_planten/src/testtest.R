## Load packages
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(DBI) # Databanken bevragen
library(inborutils)
library(effectclass)
library(RODBC) # Accessdatabank lezen -> OPGELET: draait alleen in 32-bit versie van R
library(readxl) # Excelbestanden importeren
library(ggplot2) # Grafieken maken
library(INBOtheme)
source("source/klimaatindicator_planten/src/function_calc_cti.R")
## Compile input data

# Connect to flora database and read tables
con <- connect_inbo_dbase(database_name = "D0021_00_userFlora")
taxon <- dbReadTable(con, "tblTaxon")
runhaar_flora <- florabank_traits(connection = con, "Ecotopensysteem") %>%
  collect()

# disconnect from database
dbDisconnect(con)

# load species temperature file
tpi <- read.csv("source/klimaatindicator_planten/data/TPI_Plants.csv", sep = ";", dec = ",") # Temperatuurgetal en neerslag (ForNaLab)

# load yeareffects file
yeareffects_1950 <- read.csv("source/klimaatindicator_planten/data/yeareffects_1950.csv") %>%
  mutate(NaamWetenschappelijk = as.character(NaamWetenschappelijk))
str(yeareffects_1950)

# Match with gbif taxonomy and combine + calculate temperature standard error
tpi_gbif <- gbif_species_name_match(df = tpi, name = "name") %>%
  filter(!is.na(scientificName)) %>%
  select(scientificName, name, temp = YearMeanMean, YearMean05, YearMean95)

flora_gbif <- gbif_species_name_match(df = taxon, name = "NaamWetenschappelijk") %>%
  filter(!is.na(scientificName)) %>%  # wat met alle floradata die geen scientific name hebben in gbif? (135 records)
  select(scientificName, NaamWetenschappelijk)

flora_tpi <- inner_join(x = tpi_gbif, y = flora_gbif, by = "scientificName") %>%
  mutate(temp_se = (YearMean95 - YearMean05) / (2 * qnorm(1 - 0.1/2))) #interval mss niet symmetrisch rond temp + coverage tss 5% en 95% is 90%
str(flora_tpi)

# species in temperature dataset and yeareffect dataset must be the same
flora_tpi_effects <- flora_tpi %>%
  filter(NaamWetenschappelijk %in% yeareffects_1950$NaamWetenschappelijk) %>%
  mutate(NaamWetenschappelijk = as.factor(NaamWetenschappelijk)) %>% # moet voor filter type "chr" zijn, anders kloppen levels niet
  arrange(NaamWetenschappelijk)
str(flora_tpi_effects)

yeareffects_1950_tpi <- yeareffects_1950 %>%
  filter(NaamWetenschappelijk %in% flora_tpi$NaamWetenschappelijk) %>%
  mutate(NaamWetenschappelijk = as.factor(NaamWetenschappelijk)) %>%
  arrange(NaamWetenschappelijk)
str(yeareffects_1950_tpi)

# add habitat groups based on Runhaar ecotopes
runhaar_ecotopen <- read_xlsx("C:/R/CTI/florabank-analysis/prj_bookdown/msi_vascular_plants/_data/planten_ecosystemen.xlsx", sheet = "Runhaar ecotopen", range = "A1:E96") %>%
  select(Code, habitatgroep)
spp_runhaar <- runhaar_flora %>%
  select(TaxonID, NaamWetenschappelijk = TaxonWetenschappelijk, Code) %>%
  inner_join(runhaar_ecotopen, by = "Code") %>%
  distinct(TaxonID, NaamWetenschappelijk, habitatgroep) %>%
  filter(!is.na(habitatgroep))
str(spp_runhaar)
habitatlijst <- distinct(.data = spp_runhaar, habitatgroep)
habitatlijst

# Calculate relative CTI results
CTI_1950_rel <- calc_cti(
  traitdata = flora_tpi_effects, # dataframe met NaamWetenschappelijk, temp en temp_se (1 rij per soort)
  yeareffectdata = yeareffects_1950_tpi, # dataframe met de modelschattingen van de soorten
  nrep = 1000, # aantal simulaties (1000 is voldoende)
  interval = 0.95, # betrouwbaarheidsniveau (1 - significantieniveau)
  relatief = TRUE)
CTI_1950_rel
CTI_1950_rel <- CTI_1950_rel %>%
  mutate(Jaar = as.integer(Jaar))
str(CTI_1950_rel)

# Calculate absolute CTI results
CTI_1950_abs <- calc_cti(
  traitdata = flora_tpi_effects, # dataframe met NaamWetenschappelijk, temp en temp_se (1 rij per soort)
  yeareffectdata = yeareffects_1950_tpi, # dataframe met de modelschattingen van de soorten
  nrep = 1000, # aantal simulaties (1000 is voldoende)
  interval = 0.95, # betrouwbaarheidsniveau (1 - significantieniveau)
  relatief = FALSE)

CTI_1950_abs <- CTI_1950_abs %>%
  mutate(Jaar = as.integer(Jaar))
str(CTI_1950_abs)

# plot results
CTI_1950 <- CTI_1950_abs %>%
  mutate(methode = "absoluut") %>%
  bind_rows(CTI_1950_rel %>%
              mutate(methode = "relatief"))

plot_CTI_1950 <- CTI_1950 %>%
  ggplot(aes(x = Jaar, y = cti_est)) +
  stat_fan(aes(link_sd = cti_se)) +
  geom_line(size = 1.2) +
  #scale_x_discrete(breaks = as.character(seq(1950, 2020, by = 5))) +
  labs(x = "Jaar", y = "Community Temperature Index (°C)") +
  facet_wrap(~methode)
plot_CTI_1950


# Calculate relative CTI results per habitat
flora_tpi_effects_hab <- flora_tpi_effects %>%
  inner_join(spp_runhaar, by = "NaamWetenschappelijk")
yeareffects_1950_tpi_hab <- yeareffects_1950_tpi %>%
  inner_join(spp_runhaar, by = "NaamWetenschappelijk")


flora_tpi_effects_bos <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "bos")
flora_tpi_effects_urb <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "urbaan")
flora_tpi_effects_gras <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "grasland")
flora_tpi_effects_moeras <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "moeras")
flora_tpi_effects_akker <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "akker")
flora_tpi_effects_heide <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "heide")
flora_tpi_effects_zout <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "brakke tot zilte milieus")
flora_tpi_effects_duin <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "duinen")
flora_tpi_effects_zoet <- flora_tpi_effects_hab %>%
  filter(habitatgroep == "zoet water")

yeareffects_1950_tpi_bos <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "bos")
yeareffects_1950_tpi_urb <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "urbaan")
yeareffects_1950_tpi_gras <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "grasland")
yeareffects_1950_tpi_moeras <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "moeras")
yeareffects_1950_tpi_akker <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "akker")
yeareffects_1950_tpi_heide <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "heide")
yeareffects_1950_tpi_zout <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "brakke tot zilte milieus")
yeareffects_1950_tpi_duin <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "duinen")
yeareffects_1950_tpi_zoet <- yeareffects_1950_tpi_hab %>%
  filter(habitatgroep == "zoet water")

CTI_1950_rel_bos <- calc_cti(
  traitdata = flora_tpi_effects_bos,
  yeareffectdata = yeareffects_1950_tpi_bos,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "bos")

CTI_1950_rel_urb <- calc_cti(
  traitdata = flora_tpi_effects_urb,
  yeareffectdata = yeareffects_1950_tpi_urb,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "urbaan")

CTI_1950_rel_gras <- calc_cti(
  traitdata = flora_tpi_effects_gras,
  yeareffectdata = yeareffects_1950_tpi_gras,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "grasland")

CTI_1950_rel_moeras <- calc_cti(
  traitdata = flora_tpi_effects_moeras,
  yeareffectdata = yeareffects_1950_tpi_moeras,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "moeras")

CTI_1950_rel_akker <- calc_cti(
  traitdata = flora_tpi_effects_akker,
  yeareffectdata = yeareffects_1950_tpi_akker,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "akker")

CTI_1950_rel_heide <- calc_cti(
  traitdata = flora_tpi_effects_heide,
  yeareffectdata = yeareffects_1950_tpi_heide,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "heide")

CTI_1950_rel_zout <- calc_cti(
  traitdata = flora_tpi_effects_zout,
  yeareffectdata = yeareffects_1950_tpi_zout,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "brakke tot zilte milieus")

CTI_1950_rel_duin <- calc_cti(
  traitdata = flora_tpi_effects_duin,
  yeareffectdata = yeareffects_1950_tpi_duin,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "duinen")

CTI_1950_rel_zoet <- calc_cti(
  traitdata = flora_tpi_effects_zoet,
  yeareffectdata = yeareffects_1950_tpi_zoet,
  nrep = 1000,
  interval = 0.95,
  relatief = TRUE) %>%
  mutate(habitatgroep = "zoet water")

CTI_1950_rel_hab <- bind_rows(CTI_1950_rel_bos, CTI_1950_rel_urb, CTI_1950_rel_gras, CTI_1950_rel_moeras, CTI_1950_rel_akker, CTI_1950_rel_heide, CTI_1950_rel_zout, CTI_1950_rel_duin, CTI_1950_rel_zoet) %>%
  mutate(cti_se = (cti_upr - cti_est)/qnorm(1 - 0.05/2), Jaar = as.integer(Jaar)) %>%
  arrange(Jaar, habitatgroep)

# Calculate absolute CTI results per habitat
CTI_1950_abs_bos <- calc_cti(
  traitdata = flora_tpi_effects_bos,
  yeareffectdata = yeareffects_1950_tpi_bos,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "bos")

CTI_1950_abs_urb <- calc_cti(
  traitdata = flora_tpi_effects_urb,
  yeareffectdata = yeareffects_1950_tpi_urb,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "urbaan")

CTI_1950_abs_gras <- calc_cti(
  traitdata = flora_tpi_effects_gras,
  yeareffectdata = yeareffects_1950_tpi_gras,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "grasland")

CTI_1950_abs_moeras <- calc_cti(
  traitdata = flora_tpi_effects_moeras,
  yeareffectdata = yeareffects_1950_tpi_moeras,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "moeras")

CTI_1950_abs_akker <- calc_cti(
  traitdata = flora_tpi_effects_akker,
  yeareffectdata = yeareffects_1950_tpi_akker,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "akker")

CTI_1950_abs_heide <- calc_cti(
  traitdata = flora_tpi_effects_heide,
  yeareffectdata = yeareffects_1950_tpi_heide,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "heide")

CTI_1950_abs_zout <- calc_cti(
  traitdata = flora_tpi_effects_zout,
  yeareffectdata = yeareffects_1950_tpi_zout,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "brakke tot zilte milieus")

CTI_1950_abs_duin <- calc_cti(
  traitdata = flora_tpi_effects_duin,
  yeareffectdata = yeareffects_1950_tpi_duin,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "duinen")

CTI_1950_abs_zoet <- calc_cti(
  traitdata = flora_tpi_effects_zoet,
  yeareffectdata = yeareffects_1950_tpi_zoet,
  nrep = 1000,
  interval = 0.95,
  relatief = FALSE) %>%
  mutate(habitatgroep = "zoet water")

CTI_1950_abs_hab <- bind_rows(CTI_1950_abs_bos, CTI_1950_abs_urb, CTI_1950_abs_gras, CTI_1950_abs_moeras, CTI_1950_abs_akker, CTI_1950_abs_heide, CTI_1950_abs_zout, CTI_1950_abs_duin, CTI_1950_abs_zoet) %>%
  mutate(cti_se = (cti_upr - cti_est)/qnorm(1 - 0.05/2), Jaar = as.integer(Jaar)) %>%
  arrange(Jaar, habitatgroep)

# Plot results per habitat

plot_CTI_1950_rel_hab <- CTI_1950_rel_hab %>%
  ggplot(aes(x = Jaar, y = cti_est)) +
  stat_fan(aes(link_sd = cti_se)) +
  geom_line(size = 1.2) +
  #scale_x_discrete(breaks = as.character(seq(1950, 2020, by = 10))) +
  labs(x = "Jaar", y = "Community Temperature Index (°C)") +
  facet_wrap(~habitatgroep)
plot_CTI_1950_rel_hab

plot_CTI_1950_abs_hab <- CTI_1950_abs_hab %>%
  ggplot(aes(x = Jaar, y = cti_est)) +
  stat_fan(aes(link_sd = cti_se)) +
  geom_line(size = 1.2) +
  #scale_x_discrete(breaks = as.character(seq(1950, 2020, by = 10))) +
  labs(x = "Jaar", y = "Community Temperature Index (°C)") +
  facet_wrap(~habitatgroep)
plot_CTI_1950_rel_hab
