########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code aggregates imputed data (saved by state) into one file. 
## It also makes aggregations by year, grade, etc.
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
  lapply(., fread) %>% rbindlist(., fill = T) -> data

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
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72"),]

data_shp <- merge(data, shp)



##aggregate across grade
data_by_grade <- data[leaidC %in% as.numeric(unique(shp$GEOID)),.(mean_achievement = mean(mean_achievement),
                         imputed_step_1 = mean(imputed_step_1),
                         imputed_step_2 = mean(imputed_step_2)), by = .(leaidC,leanm, fips, stateabb, subgroup, grade)]


##aggregate across eyars
data_by_year<- data[leaidC %in%  as.numeric(unique(shp$GEOID)),.(mean_achievement = mean(mean_achievement),
                          imputed_step_1 = mean(imputed_step_1),
                          imputed_step_2 = mean(imputed_step_2)), by = .(leaidC,leanm, fips, stateabb, subgroup, year)]





##aggregate across grade and years
data_collapsed <- data[leaidC %in% as.numeric(unique(shp$GEOID)),.(mean_achievement = mean(mean_achievement),
                       imputed_step_1 = mean(imputed_step_1),
                       imputed_step_2 = mean(imputed_step_2)), by = .(leaidC,leanm, fips, stateabb, subgroup)]



data_collapsed_shp <- merge(data_collapsed, shp)
data_by_year_shp <- merge(data_by_year, shp)
data_by_grade_shp <- merge(data_by_grade, shp)

fwrite(data_collapsed[subgroup == c.subset],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/data_collapsed_", c.subset, ".csv"))
fwrite(data_by_year[subgroup == c.subset],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/data_by_year_", c.subset, ".csv"))
fwrite(data_by_grade[subgroup == c.subset],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/data_by_grade_", c.subset, ".csv"))


