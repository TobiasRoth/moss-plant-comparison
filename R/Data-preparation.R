rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)

# Connection to data base
db <- src_sqlite(path = "DB/DB_BDM_2019_08_18.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dat <- 
  
  # Select surveys
  tbl(db, "KD_Z9") %>% 
  filter(HN != "Aecker" & HN != "Gletscher, Wasser" & HN != "Siedlung") %>% 
  filter(!is.na(yearMoos) & !is.na(yearPl)) %>% 
  filter(Aufnahmetyp == "Normalaufnahme_Z9" | Aufnahmetyp == "BDM_LANAG_Normalaufnahme_Z9") %>% 
  transmute(
    aID_KD = aID_KD, 
    aID_STAO = aID_STAO, 
    year_pl = yearPl, 
    year_mo = yearMoos, 
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
        T_pl = mean(T, na.rm = TRUE),
        N_pl = mean(N, na.rm = TRUE)
      )) %>% 
  
  # Add moss data:
  left_join(
    tbl(db, "Moos") %>% 
      left_join(tbl(db, "Traits_Moos")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_mo = n(),
        T_mo = mean(T, na.rm = TRUE),
        N_mo = mean(N, na.rm = TRUE))) %>% 
  as_tibble() %>% 
  replace_na(list(AZ_pl = 0, AZ_mo = 0)) 

# Rename land-use types
dat$land_use[dat$land_use == "Nicht genutzte Flaechen"] <- "unused"
dat$land_use[dat$land_use == "Alpweiden"] <- "grassland"
dat$land_use[dat$land_use == "Wiesen, Weiden"] <- "grassland"
dat$land_use[dat$land_use == "Wald"] <- "forest"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save data for further analyses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove survey ID
dat <- dat %>% 
  select(-aID_KD)

# Replace CoordID with siteID
dat$aID_STAO <- as.integer(factor(dat$aID_STAO))

# Save data
save(dat, file = "Data-raw/dat.RData")

