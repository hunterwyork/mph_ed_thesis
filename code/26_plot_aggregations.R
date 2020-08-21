########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code plots aggregated data.
########################################################################

library(stringr)
library(rlang)
library(data.table)
library(sf)
library(dplyr)
library(INLA)
library(spdep)
library(boot)
library(ggplot2)
library(gridExtra)
library(parallel)


cores <- 10

#read data
paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/") %>% list.files(., full.names = T, pattern = "data_collapsed") %>% 
  lapply(., fread) %>% rbindlist() -> data_collapsed
paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/") %>% list.files(., full.names = T, pattern = "data_by_grade") %>% 
  lapply(., fread) %>% rbindlist() -> data_by_grade
paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/") %>% list.files(., full.names = T, pattern = "data_by_year") %>% 
  lapply(., fread) %>% rbindlist() -> data_by_year


##
data_collapsed[imputed_step_2 >= 1,imputed_maj := 1]
data_collapsed[imputed_step_2 <1 ,imputed_maj := 0]

#load in population data for masking
#load census vars
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()
pops <- census_covs_wide[measure == "population"]
for(c.race in c("asian", "black", "hispanic", "white")){
  pops[get(c.race) < 100, (paste0("mask_", c.race)) := 1]
  pops[is.na(get(paste0("mask_", c.race))), (paste0("mask_", c.race)) := 0]
}
pops[, mask_all := 0]
pops_melt <- melt(pops[,.(GEOID, mask_asian, mask_black, mask_white, mask_hispanic, mask_all)], id.var = "GEOID")
pops_melt[, subgroup := tstrsplit(variable, "_",keep = 2)]
#read ideal full list of school dists
shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")

# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/2013_Unified_Elementary_SD.shp" %>% 
#   read_sf() -> shp

#add blank rows for school districts missing from data
shp$fips <- as.numeric(shp$STATEFP)
#shp$leaidC <- as.numeric(str_sub(shp$GEOID), 3, -1)
shp$leaidC <- as.numeric((shp$GEOID))

shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")

#get rid of AK, HI, and PR
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72", "02", "15"),]
shp <- shp[shp$HIGRADE == "12",]

#load state shapefile for some borders
states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)
states_shp <- states_shp[states_shp$STATEFP %in% unique(shp$STATEFP),]


data_collapsed <- merge(data_collapsed, pops_melt, by.x = c("leaidC", "subgroup"), by.y = c("GEOID", "subgroup"), all.x = T)
data_collapsed_shp <- merge(data_collapsed, shp, by = "leaidC", all.x = T)


data_by_year_shp <- merge(data_by_year, shp)
data_by_grade_shp <- merge(data_by_grade, shp)






gg1 <- ggplot(data_collapsed_shp[!(value == 1) & subgroup %in% c("all", "asian", "black", "hispanic", "white")]) + 
  geom_sf(aes(fill = mean_achievement, geometry = geometry, color = as.factor(imputed_maj)), lwd = .1) +
  scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
  scale_color_manual(values = c(NA, "red"))+
  facet_wrap(~subgroup)+
  ggtitle(paste0(c.subset)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Grade Levels Above/Below National Average") + 
  theme(legend.position = "bottom") 
print(gg1)



###################
plottr <- function(c.subset){
  pdf(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/maps_seda_imputed_agg/collapsed/", c.subset, "_collapsed.pdf"), height = 12, width = 16)
  
  gg1 <- ggplot(data_collapsed_shp[subgroup == c.subset & !leaidC %in% pops[get(paste0("mask_", c.subset)) == 1, GEOID]]) + 
    geom_sf(aes(fill = mean_achievement, geometry = geometry, color = as.factor(imputed_maj)), lwd = .1) +
    scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
    scale_color_manual(values = c(NA, "red"))+
    ggtitle(paste0(c.subset)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "Grade Levels Above/Below National Average") + 
    theme(legend.position = "bottom") 
  print(gg1)
  
  dev.off()
}

lapply( c("asian", "black", "hispanic", "white", "all"), plottr)


plottr <- function(c.subset){
  pdf(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/maps_seda_imputed_agg/by_year/", c.subset, "_by_year.pdf"), height = 10, width = 14)
  
  for (c.year in  2009:2016){
    gg1 <- ggplot(data_by_year_shp[subgroup == c.subset & !leaidC %in% pops[get(paste0("mask_", c.subset)) == 1, GEOID] & year == c.year]) + 
      geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
      geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.subset, ", ", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    print(gg1)
  }
  
  
  dev.off()
}
mclapply( c("asian", "black", "hispanic", "white", "all"), plottr, mc.cores = cores)
