########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code plots imputed data.
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

task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
codes <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/seda_codes.csv")
c.subset <- codes[task_id, code]

#read data
paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data/") %>% list.files(., full.names = T, pattern = c.subset) %>% 
  lapply(., fread) %>% rbindlist() -> data

#read ideal full list of school dists
shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")
# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/2013_Unified_Elementary_SD.shp" %>% 
#   read_sf() -> shp

#add blank rows for school districts missing from data
shp$fips <- as.numeric(shp$STATEFP)
#shp$leaidC <- as.numeric(str_sub(shp$GEOID), 3, -1)
shp$leaidC <- as.numeric((shp$GEOID))
shp <- shp[shp$HIGRADE == "12",]

shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")

#get rid of AK, HI, and PR
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72", "02", "15"),]

data_shp <- merge(data, shp)

#load state shapefile for some borders
states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)
states_shp <- states_shp[states_shp$STATEFP %in% unique(shp$STATEFP),]

c.grade <- 3
c.year <- 2015
#plot all vals

plottr <- function(c.grade){
  for(c.year in unique(data$year)){
    jpeg(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/maps_seda_imputed/", c.subset,"_",c.grade, "_", c.year, ".jpeg"), height = 700, width = 500)
    print(c.grade)
    print(c.year)
    
    gg1 <- ggplot(data_shp[grade == c.grade & subject == "math" & year == c.year]) + 
      geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
      geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.subset,"\nMath", ", Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    gg2 <- ggplot(data_shp[grade == c.grade & subject == "ela" & year == c.year]) + 
      geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
      geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.subset, "\nReading, ", ", Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    grid.arrange(gg1, gg2, nrow = 2)
    dev.off()
  }
}
mclapply(unique(data$grade), plottr, mc.cores = cores)

plottr_pdf <- function(c.grade){
  for(c.year in unique(data$year)){
    print(c.grade)
    print(c.year)
    gg1 <- ggplot(data_shp[grade == c.grade & subject == "math" & year == c.year]) + 
      geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
      geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.subset,"\nMath", ", Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    gg2 <- ggplot(data_shp[grade == c.grade & subject == "ela" & year == c.year]) + 
      geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = 0, color = NA) +
      scale_fill_gradient2(breaks = seq(-4,4, 1), limits = c(-4, 4), low = "red", high = "blue", mid = "gray90") +
      geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
      ggtitle(paste0(c.subset, "\nReading, ", "Grade: ", c.grade, ", Year:", c.year)) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
      labs(fill = "Percent of the Population Achieving Proficiency") + 
      theme(legend.position = "bottom") 
    grid.arrange(gg1, gg2, nrow = 2)
  }
}

pdf(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/maps_seda_imputed_pdf/", c.subset, ".pdf"), height = 15, width = 20)
lapply(unique(data$grade)[1:2], plottr_pdf)
dev.off()





