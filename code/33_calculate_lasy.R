########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code takes final outputs: learning data, and high school retention
## data, combines them into metrics of years of schooling, learning-adj
## usted years of schooling, and grades above or below expected (learning).
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

paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent/") %>% list.files(., full.names = T) %>% 
  lapply(., fread) %>% rbindlist(., fill = T) -> retention
retention <- retention[subgroup %in% c("asian", "all", "white", "black", "hispanic", "native")]

setnames(retention, "mean_achievement", "retention")

retention[retention < 0, retention := 0]
retention[retention >1, retention := 1]



paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/aggregations/") %>% 
  list.files(., pattern = "data_by_year", full.names = T) %>% 
  lapply(fread) %>% 
  rbindlist() -> learning
learning <- learning[subgroup %in% c("asian", "all", "white", "black", "hispanic", "native")]

#merge
lasy <- merge(retention[,.(leaidC, subgroup, year, retention, grade)], learning, by = c("leaidC", "subgroup", "year"), all = T)
lasy[mean_achievement < -4, mean_achievement := -4]
lasy[mean_achievement > 4, mean_achievement := 4]
lasy <- lasy[!is.na(grade)]
lasy <- dcast(lasy, leaidC + subgroup + year + mean_achievement + imputed_step_1 + imputed_step_2 + stateabb + fips ~ grade, value.var = "retention")

lasy[, lasy := (10 + (`10`*`11`*`12`) + (`10`*`11`) + (`10`)) + mean_achievement]
lasy[,yos := (10 + (`10`*`11`*`12`) + (`10`*`11`) + (`10`)) ]
lasy[lasy <= 13, lasy_capped := lasy]
lasy[lasy > 13, lasy_capped := 13]




dir.create("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/lasy_aggregations/")
fwrite(lasy, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/lasy_aggregations/all.csv")
