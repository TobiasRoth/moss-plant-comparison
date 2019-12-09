rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)

# Connection to data base
db <- src_sqlite(path = "DB/DB_BDM_2019_08_18.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export survey data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# One line in "dat" contains a survey (each plot is surveyed once every five
# years). Surveys are included only if plant and moss surveys were considered
# vallid with sufficient data quality.

dat <- 
  
  # Select surveys
  tbl(db, "KD_Z9") %>% 
  filter(HN != "Aecker" & HN != "Gletscher, Wasser" & HN != "Siedlung") %>% 
  filter(!is.na(yearMoos) & !is.na(yearPl)) %>% 
  filter(Aufnahmetyp == "Normalaufnahme_Z9" | Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9") %>% 
  transmute(
    aID_KD = aID_KD, 
    aID_STAO = aID_STAO, 
    year = yearPl,
    land_use = HN) %>% 
  
  # Add site data:
  left_join(
    tbl(db, "Raumdaten_Z9") %>% 
      transmute(
        aID_STAO = aID_STAO, 
        elevation = Hoehe,
        HS = HS
      )) %>% 
  
  # Add plant data:
  left_join(
    tbl(db, "Pl") %>% 
      left_join(tbl(db, "TRAITS_PL")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_pl = n(),
        T_pl = mean(T, na.rm = TRUE) %>% round(2)
      )) %>% 
  
  # Add moss data:
  left_join(
    tbl(db, "Moos") %>% 
      left_join(tbl(db, "Traits_Moos")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_mo = n(),
        T_mo = mean(T, na.rm = TRUE) %>% round(2)
      )) %>% 
  as_tibble() %>% 
  replace_na(list(AZ_pl = 0, AZ_mo = 0)) 

# Rename land-use types
dat$land_use[dat$land_use == "Nicht genutzte Flaechen"] <- "unused"
dat$land_use[dat$land_use == "Alpweiden"] <- "grassland"
dat$land_use[dat$land_use == "Wiesen, Weiden"] <- "grassland"
dat$land_use[dat$land_use == "Wald"] <- "forest"

# Rename HS types
dat$HS[dat$HS == "kollin"] <- "colline"
dat$HS[dat$HS == "montan"] <- "montane"
dat$HS[dat$HS == "subalpin"] <- "subalpine"
dat$HS[dat$HS == "alpin"] <- "alpine"

# Remove 1 outlier (Plot with 1 cryophilous bryophyte species at 358 asl, in the
# floodplain of the Maggia river, the moss presumbably has been floated from
# further above)
dat <- dat[dat$aID_KD!=3090399990,]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export plant and moss data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Mosses
moss <- 
  tbl(db, "Moos") %>% 
  left_join(tbl(db, "ARTEN") %>% select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Moos")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, dat$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

# plants
plants <- 
  tbl(db, "PL") %>% 
  left_join(tbl(db, "ARTEN") %>% select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Pl")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, dat$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Summary statistics ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summarystat <- dat %>% 
  group_by(HS) %>% 
  dplyr::summarise(
    N_sites = n_distinct(aID_STAO),
    N_surveys = n(),
    moos_SR = mean(AZ_mo) %>% round(2),
    plants_SR = mean(AZ_pl) %>% round(2)
  ) %>% 
  left_join(
    moss %>% left_join(dat) %>% 
      group_by(HS) %>% 
      dplyr::summarise(
        moss_tot = n_distinct(aID_SP)
      )) %>% 
  left_join(
    plants %>% left_join(dat) %>% 
      group_by(HS) %>% 
      dplyr::summarise(
        plants_tot = n_distinct(aID_SP)
      ))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save data for further analyses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove aID_KD
dat <- dat[, -1]

# New aID_STAO
dat$aID_STAO <- as.integer(factor(dat$aID_STAO))
dat <- dat %>% arrange(aID_STAO, year)

# Save data
write_csv(dat, path = "Data-raw/surveys.csv")
write_csv(summarystat, path = "Data-raw/summarystat.csv")







