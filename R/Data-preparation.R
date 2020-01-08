rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(RColorBrewer)
library(BDM)
library(simba)
library(raster)

# Connection to data base
db <- src_sqlite(path = "DB/DB_BDM_2019_08_18.db", create = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export survey data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# One line in "surveys" contains a survey (each plot is surveyed once every five
# years). Surveys are included only if vascular plant and bryophyte surveys were
# considered valid with sufficient data quality.

surveys <- 
  
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
  
  # Add vascular plant data:
  left_join(
    tbl(db, "Pl") %>% 
      left_join(tbl(db, "TRAITS_PL")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_pl = n(),
        T_pl = mean(T, na.rm = TRUE) %>% round(2)
      )) %>% 
  
  # Add bryophyte data:
  left_join(
    tbl(db, "Moos") %>% 
      left_join(tbl(db, "Traits_Moos")) %>% 
      group_by(aID_KD) %>% 
      dplyr::summarise(
        AZ_mo = n(),
        T_mo = mean(T, na.rm = TRUE) %>% round(2),
        T_mo_sh = mean(T[GenTime <= 6.6 & !is.na(GenTime)], na.rm = TRUE),
        T_mo_lo = mean(T[GenTime > 6.6 & !is.na(GenTime)], na.rm = TRUE)
      )) %>% 
  as_tibble() %>% 
  replace_na(list(AZ_pl = 0, AZ_mo = 0)) 

# Rename land-use types
surveys$land_use[surveys$land_use == "Nicht genutzte Flaechen"] <- "unused"
surveys$land_use[surveys$land_use == "Alpweiden"] <- "grassland"
surveys$land_use[surveys$land_use == "Wiesen, Weiden"] <- "grassland"
surveys$land_use[surveys$land_use == "Wald"] <- "forest"

# Rename HS types
surveys$HS[surveys$HS == "kollin"] <- "colline"
surveys$HS[surveys$HS == "montan"] <- "montane"
surveys$HS[surveys$HS == "subalpin"] <- "subalpine"
surveys$HS[surveys$HS == "alpin"] <- "alpine"

# Remove plots with land_use = unused in the colline, montane and subalpine zone
# These are special cases (e.g. gravel pits, waste lands) and results can hardly
# be interpreted.
surveys <- surveys %>% 
  filter(!(land_use == "unused" & HS == "colline")) %>% 
  filter(!(land_use == "unused" & HS == "montane"))

# Remove 1 outlier (Plot with 1 cryophilous bryophyte species at 358 asl, in the
# floodplain of the Maggia river, the moss presumbably has been floated from
# further above)
surveys <- surveys[surveys$aID_KD!=3090399990,]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export vascular plant and bryophyte data from DB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bryophytes
moss <- 
  tbl(db, "Moos") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Moos")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

# Vascular plants
plants <- 
  tbl(db, "PL") %>% 
  left_join(tbl(db, "KD_Z9")) %>% 
  left_join(tbl(db, "ARTEN") %>% dplyr::select(aID_SP, Gattung, Art)) %>%      
  left_join(tbl(db, "Traits_Pl")) %>% 
  filter(!is.na(aID_SP)) %>% 
  as_tibble() %>% 
  filter(!is.na(match(aID_KD, surveys$aID_KD))) %>% 
  transmute(
    aID_KD = aID_KD,
    aID_STAO = aID_STAO,
    aID_SP = aID_SP,
    species = paste(Gattung, Art)
  ) %>% 
  arrange(aID_KD, aID_SP)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare site data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get temporal trend
get_trend <- function(mes, year) {
  res <- NA
  try({
    dd <- tibble(y = mes, yr = (year -2010)/10) 
    mod <- lm(y ~ yr, data = dd)
    res <- coef(mod)[2]
  })
  res
}

# Calculate averages and temporal trends per site
dat <- surveys %>% 
  group_by(aID_STAO) %>% 
  dplyr::summarise(
    elevation = mean(elevation),
    HS = first(HS),
    land_use = first(land_use),
    SR_pl_mean = mean(AZ_pl, na.rm = TRUE),
    SR_pl_trend = get_trend(AZ_pl, year),
    T_pl_mean = mean(T_pl, na.rm = TRUE),
    T_pl_trend = get_trend(T_pl, year),
    SR_mo_mean = mean(AZ_mo, na.rm = TRUE),
    SR_mo_trend = get_trend(AZ_mo, year),
    T_mo_mean = mean(T_mo, na.rm = TRUE),
    T_mo_sh_mean = mean(T_mo_sh, na.rm = TRUE),
    T_mo_lo_mean = mean(T_mo_lo, na.rm = TRUE),
    T_mo_trend = get_trend(T_mo, year),
    T_mo_sh_trend = get_trend(T_mo_sh, year),
    T_mo_lo_trend = get_trend(T_mo_lo, year)
  ) 

# Turnover between two survey from the same year
getturnover <- function(x, specdat) {
  res <- NA
  tt <- specdat %>% 
    filter(aID_STAO == dat$aID_STAO[x]) 
  if(n_distinct(tt$aID_KD) > 1 & n_distinct(tt$aID_SP) > 1) {
    res <- 
      tt %>% 
      transmute(aID_KD = aID_KD, aID_SP = aID_SP, Occ = 1) %>% 
      simba::sim(method = "cocogaston", listin = TRUE, listout = TRUE) %>% 
      pull(cocogaston) %>% mean
  }
  res
}

# Apply functions for all sites, independently for bryophytes and vascular
# plants
dat$TU_mo <-  map_dbl(1:nrow(dat), getturnover, specdat = moss)
dat$TU_pl <-  map_dbl(1:nrow(dat), getturnover, specdat = plants)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Summary statistics ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
stats <- list()

# Total number of recorded species
stats$Tot_N_Spec_mo <- n_distinct(moss$aID_SP)
stats$Tot_N_Spec_pl <- n_distinct(plants$aID_SP)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make map with location of study plots ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load spatial data for map
load("Data-geo/ch.RData")
load("Data-geo/gadm.RData")
load("Data-geo/seen.RData")

# Colorsetting  
seecol <- brewer.pal(8, name = "Paired")[1]
sitecol <- brewer.pal(8, name = "Paired")[4]
sitecolEPT <- brewer.pal(8, name = "Paired")[8]

pdf("Manuscript/studysite.pdf", width = 6, height = 4)
par(mar = c(0,0,2,0))
plot(NA, xlim = c(490000, 840000), ylim = c(60000, 300000), type = "n", axes = FALSE, asp = 1)
plot(ch, add =TRUE)
plot(gadm, add = TRUE)
plot(seen[1:13,], add = TRUE, col = seecol, border = seecol, lwd = 0.01)
points(coordID2coord(dat$aID_STAO), pch = 16, cex = 0.5)
xx <- 490000
yy <- 80000
lines(x=c(xx, xx+50000), y=c(yy, yy))
lines(x=c(xx, xx), y=c(yy, yy+2000))
lines(x=c(xx+50000, xx+50000), y=c(yy, yy+2000))
text(xx+25000, yy-5000, "50 km", cex=0.7)
dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save data for further analyses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# New aID_STAO
surveys$aID_STAO <- match(surveys$aID_STAO, dat$aID_STAO)
surveys <- surveys[, -1]
dat$aID_STAO <- 1:nrow(dat)

# Save data
write_csv(surveys, path = "Data-raw/surveys.csv")
write_csv(dat, path = "Data-raw/dat.csv")
save(stats, file = "Data-raw/stats.RData")
