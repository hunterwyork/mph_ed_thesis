########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code prepares high school population stock data, turns it into
## a modelable entity (probability of advancement) and merges this data
## on covariate data.
########################################################################

library(stringr)
library(haven)
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)

# #hs grad data
# dt <- fread('/home/j/temp/hyork/nationwidegradrates.csv')
# dt[, grad_rate := as.numeric(str_sub(`Graduation Rate`, 1, -2))/100]
# dt[, `Graduation Rate` := NULL]
# 
# #merge on fips
# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/fip_map.csv" %>% fread -> fip_map
# dt <- merge(dt, fip_map, all.x= T, by.x = "State", by.y = "stateabb")
# 
# #read ideal full list of school dists
# shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")
# 
# #add blank rows for school districts missing from data
# shp$fips <- as.numeric(shp$STATEFP)
# shp$leaidC <- (shp$UNSDLEA)
# shp$ELSDLEA <- as.numeric(shp$ELSDLEA)
# shp$leaidC <- as.numeric(paste0(shp$fips, shp$leaidC))
# shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")
# #rename special school districts to be the same as normal school districts
# shp$leaidC[is.na(shp$leaidC)] <- as.numeric(shp$ELSDLEA[is.na(shp$leaidC)])
# 
# #get rid of AK, HI, and PR
# shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72", "02", "15"),]
# 
# #
# shp_dt <- data.table(shp)
# fips_dt <- shp_dt[,.(NAME, STATEFP, UNSDLEA)]
# fips_dt[, leaidC := as.numeric(paste0(STATEFP, UNSDLEA))]
# 
# #intersect names of dists with shpfile
# setnames(fips_dt, "STATEFP", "fips")
# fips_dt[, fips := as.numeric(fips)]
# setnames(dt, "District name", "NAME")
# dt_merge_perf <- merge(fips_dt, dt[NAME %in% fips_dt$NAME], by = c("NAME", "fips"), all.y = T)
# 
# #fuzzy match the rest
# fuzz <- dt[!NAME %in% fips_dt$NAME]
# matchr <- function(c.name, c.shp, c.name_dt){
#   print(c.name)
#   c.fip <- c.name_dt[NAME == c.name, fips]
#   i <- .3
#   while(
#     length(
#       agrep(
#         gsub("School District|school district|public schools|Public Schools", "", c.name),
#         c.shp[fips == c.fip, NAME], max.distance = i, value = T) 
#     ) > 1
#   ){
#     print(i)
#     i <- i- .01
#   }
#   c.new_name <- agrep(gsub("School District|school district|public schools|Public Schools", "", c.name),
#   c.shp[fips == c.fip, NAME], max.distance = i, value = T)
#   return(c.new_name)
# }
# match_list <- lapply(fuzz$NAME, matchr, c.shp = fips_dt, c.name_dt = fuzz)
# 
# ###
# hs_comp <- fread("/home/j/temp/hyork/high_school_completion/nhgis0007_csv/nhgis0007_ds123_1990_blck_grp.csv")
# hs_comp <- hs_comp[,.SD, .SDcols = c("GISJOIN", "COUNTY", "STATE", paste0("E", 39001:39065))]
# hs_comp <- melt(hs_comp, id.vars = c("GISJOIN", "COUNTY", "STATE"))
# 
# #load leaid block group mapping
# 
# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/grf19_lea_blkgrp.xlsx" %>% 
#   readxl::read_xlsx() %>% data.table() -> blk_leaid_map
# blk_leaid_map[, GISJOIN := paste0("G", BLKGRP)]
# 
# merge(hs_comp, blk_leaid_map, by = "GISJOIN", all.x = T)[!is.na(leaidC)]

"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/" %>% 
  list.files(., pattern = "ag1|dr0", full.names = T) %>% 
  c(.,"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1415_w_0216161a.txt") %>% 
  lapply(read.delim) %>%
  rbindlist(., fill = T) %>% 
  data.table()-> ccd_early

rbind(fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1516_w_1a_011717.csv"),
      ccd_early, fill = T) -> ccd_early

ccd_early_melt <- melt(ccd_early, 
                       id.vars = c("SURVYEAR", "LEAID", "FIPST", "NAME", "MSTATE", "MZIP", "CONUM"))

ccd_early_melt <- ccd_early_melt[variable %like% "^G[0-9][0-9]$|[0-9]M$|[0-9]F$|^KG$|KGM$|KGF$"]
ccd_early_melt[variable %in% c("KG", paste0("G0", 1:9), paste0("G", 10:12)), subgroup := "all"]
ccd_early_melt[variable %like% "^AM", subgroup := "native"]
ccd_early_melt[variable %like% "^AS", subgroup := "asian"]
ccd_early_melt[variable %like% "^HI", subgroup := "hispanic"]
ccd_early_melt[variable %like% "^BL", subgroup := "black"]
ccd_early_melt[variable %like% "^WH", subgroup := "white"]
ccd_early_melt[variable %like% "M$|F$", grade := str_sub(variable, 3, 4)]
ccd_early_melt[variable == "KG", grade := "KG"]
ccd_early_melt[variable %in% c(paste0("G0", 1:9), paste0("G", 10:12)), grade := str_sub(variable, 2,3)]
ccd_early_melt <- ccd_early_melt[!is.na(subgroup)]
ccd_early_melt[value %in% c(-2, -1), value := NA]
ccd_early_melt[, value := as.numeric(value)]

ccd_early_melt[, GRADE := paste0("Grade ", grade)]
ccd_early_melt[, SCHOOL_YEAR := paste0(SURVYEAR, "-", (SURVYEAR +1))]
ccd_early_melt[, race := subgroup]
ccd_early_melt[, LEA_NAME := NAME]
ccd_early_melt[, STUDENT_COUNT := value]
#load in ccd data
c("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1617_l_2a_11212017.csv",
"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1718_l_1a_083118.csv",
"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_lea_052_1819_l_1a_091019/ccd_lea_052_1819_l_1a_091019.csv") %>% 
  lapply(fread) %>% rbindlist() -> ccd
ccd <- ccd[,.(LEAID, SCHOOL_YEAR,SEX, RACE_ETHNICITY, GRADE, STUDENT_COUNT, TOTAL_INDICATOR, LEA_NAME)]
ccd[ RACE_ETHNICITY %like% "Asian", race := "asian"]
ccd[RACE_ETHNICITY %like% "Native", race := "native"]
ccd[ RACE_ETHNICITY %like% "Black", race := "black"]
ccd[RACE_ETHNICITY %like% "Hispanic", race := "hispanic"]
ccd[RACE_ETHNICITY %like% "White", race := "white"]
ccd[,RACE_ETHNICITY := NULL]
all_races <- ccd[TOTAL_INDICATOR %like% "Subtotal 4"]
all_races[, race := "all"]
ccd <- ccd[TOTAL_INDICATOR %like% "Category Set A"]
ccd <- rbind(ccd, all_races)

#insert earlier data
ccd <- rbind(ccd, ccd_early_melt, fill = T)


#collapse
ccd_data <- ccd[,.(count = sum(STUDENT_COUNT, na.rm = T)), by = .(LEAID, SCHOOL_YEAR, GRADE, LEA_NAME, race)]
ccd_data[, grade := tstrsplit(GRADE, " ", keep = 2, type.convert = T)]
ccd_data[, year := tstrsplit(SCHOOL_YEAR, "-", keep = 1, type.convert = T)]
ccd_data[, c("GRADE", "SCHOOL_YEAR") := NULL]
ccd_data[, grade := as.numeric(grade)]
ccd_data <- ccd_data[!is.na(grade)]
ccd_data <- ccd_data[!is.na(race)]
ccd_data[, cohort := year-grade]
ccd_data <- ccd_data[order(LEAID, race, grade, year)]
ccd_data[, lag_count := lag(count), by = .(cohort, LEAID, race)]
ccd_data[, perc_diff := (count- lag_count)/lag_count]
ccd_data[is.nan(perc_diff) | is.infinite(perc_diff), perc_diff := NA]
#ccd_data[perc_diff > 1, perc_diff := 1]
ccd_data[, perc_diff_lead_1 := lead(perc_diff, 1), by = .(LEAID, race, grade)]
ccd_data[, perc_diff_lead_2 := lag(perc_diff, 1), by = .(LEAID, race, grade)]
ccd_data[, avg_diff_1_8 := mean(c(median(perc_diff[grade < 7 & lag_count > 10], na.rm = T),
           median(perc_diff_lead_1[grade < 7], na.rm = T),
           median(perc_diff_lead_2[grade < 7], na.rm = T)), na.rm = T), by = .(LEAID, race, year)]

avg_diffs <- ccd_data[!is.na(perc_diff),.(avg_perc_diff = 1+ ((perc_diff + perc_diff_lead_1 + perc_diff_lead_2)/3) - mean(avg_diff_1_8),
                                          unadj_avg_perc_diff = 1+ ((perc_diff + perc_diff_lead_1 + perc_diff_lead_2)/3),#
                         avg_pop = mean(lag_count, na.rm = T)), by = .(grade, LEAID, race, year)]
avg_diffs[is.nan(avg_perc_diff), avg_perc_diff := NA]
avg_diffs[is.nan(avg_pop), avg_pop := NA]
avg_diffs[is.infinite(avg_perc_diff), avg_perc_diff := NA]
avg_diffs[is.infinite(avg_pop), avg_pop := NA]
#avg_diffs[avg_perc_diff > 1, avg_perc_diff := 1]
avg_diffs[,avg_perc_diff_no_cap :=avg_perc_diff]
avg_diffs[avg_perc_diff > 1 & grade %in% 10:12, avg_perc_diff := 1]


avg_diffs[, `Race/Ethnicity` := paste0(toupper(str_sub(race,1,1)), str_sub(race,2,))]

avg_diffs[ avg_perc_diff < 0, avg_perc_diff := NA]
avg_diffs[avg_pop < 15, avg_perc_diff := NA]
avg_diffs <- avg_diffs[grade != 13]

gg1 <- avg_diffs[avg_pop > 100 & race != "native"& grade <= 12, mean(avg_perc_diff, na.rm = T), by = .(`Race/Ethnicity`, grade)] %>% ggplot() + 
  geom_line(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`))+
  geom_point(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`)) + 
  scale_x_continuous(breaks = seq(1,12,1)) + theme_bw() + xlab("Grade") + ylab("Retention (Percent)") + ggtitle("Adjusted")

gg2 <- avg_diffs[avg_pop > 100 &race != "native"& grade <= 12, mean(unadj_avg_perc_diff, na.rm = T), by = .(`Race/Ethnicity`, grade)] %>% ggplot() + 
  geom_line(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`))+
  geom_point(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`)) + 
  scale_x_continuous(breaks = seq(1,12,1))+ theme_bw()+ xlab("Grade") + ylab("Retention (Percent)") + ggtitle("Unadjusted")

gg3 <- avg_diffs[avg_pop > 100 & race != "native"&grade <= 12, mean(avg_perc_diff_no_cap, na.rm = T), by = .(`Race/Ethnicity`, grade)] %>% ggplot() + 
  geom_line(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`))+
  geom_point(aes(x = grade, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`)) + 
  scale_x_continuous(breaks = seq(1,12,1))+ theme_bw()+ xlab("Grade") + ylab("Retention (Percent)") + ggtitle("Adjusted no cap")

grid.arrange(gg1, gg2,gg3, nrow = 3)


retent <- avg_diffs[grade %in% 10:12 & avg_perc_diff >= 0,.(retention = prod(avg_perc_diff, na.rm = T), avg_pop = mean(avg_pop)), by= .(LEAID, race, year)]
retent[avg_pop > 100  & retention <= 1] %>% 
  ggplot() + geom_density(aes(x = retention)) + facet_wrap(~ race)

fwrite(retent, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/retention_rates_hs.csv")
fwrite(avg_diffs, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv")


shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")


 # "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/2013_Unified_Elementary_SD.shp" %>% 
 #   read_sf() -> shp


shp$fips <- as.numeric(shp$STATEFP)
shp$leaidC <- (shp$UNSDLEA)
shp$ELSDLEA <- as.numeric(shp$ELSDLEA)
shp$leaidC <- as.numeric(paste0(shp$fips, shp$leaidC))
shp <- st_transform(shp, crs = "+proj=longlat +datum=NAD83 +no_defs")
#rename special school districts to be the same as normal school districts
#shp$leaidC[is.na(shp$leaidC)] <- as.numeric(shp$ELSDLEA[is.na(shp$leaidC)])

#get rid of AK, HI, and PR
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72"),]
states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)
states_shp <- states_shp[states_shp$STATEFP %in% unique(shp$STATEFP),]


retent_shp <- merge(retent, shp, by.x = "LEAID", by.y = "leaidC", all = T)

gg1 <- ggplot(retent_shp[race == "all" & year == 2011 & avg_pop > 10]) + 
  geom_sf(aes(fill = retention, geometry = geometry), lwd = 0, color = NA) +
  scale_fill_gradient2(breaks = seq(0,1, .25), limits = c(0, 1), low = "red", high = "blue", mid = "gray90", midpoint = .5) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "white") +
  ggtitle(paste0()) + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Retention Percentage") + 
  theme(legend.position = "bottom") 
print(gg1)

