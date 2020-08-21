########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code pulls in all measured data used for this project. Data
## are sourced from SEDA, the ACS, the decennial Census, and CCD.
########################################################################

library(stringr)
library(rlang)
library(haven)
library(data.table)
library(sf)
library(dplyr)
library(INLA)
library(spdep)
library(boot)
library(gtools)
library(ggplot2)
library(gridExtra)
library(tidycensus, lib.loc = "/home/j/temp/hyork/rlibs")

########################################################################
## Pull in SEDA Data
########################################################################

#import seda data
"/home/j/temp/hyork/seda_geodist_long_gcs_v30.dta" %>% read_dta() %>% data.table()-> seda_gcs

#cast long and rename variables
seda_long <- melt(seda_gcs, id.vars = c("leaidC", "leanm", "fips", "stateabb", "grade", "year", "subject"))
seda_long[variable %like% "totgyb", measure := "sample_size"]
seda_long[variable %like% "mn", measure := "mean_achievement"]
seda_long[variable %like% "se", measure := "mean_achievement_se"]
seda_long[variable %like% "all", subgroup := "all"]
seda_long[variable %like% "asn", subgroup := "asian"]
seda_long[variable %like% "blk", subgroup := "black"]
seda_long[variable %like% "ecd", subgroup := "ec_disadv"]
seda_long[variable %like% "fem", subgroup := "female"]
seda_long[variable %like% "hsp", subgroup := "hispanic"]
seda_long[variable %like% "mal", subgroup := "male"]
seda_long[variable %like% "nam", subgroup := "native"]
seda_long[variable %like% "nec", subgroup := "non_ec_disadv"]
seda_long[variable %like% "wht", subgroup := "white"]
seda_long[variable %like% "wag", subgroup := "wh_as_gap"]
seda_long[variable %like% "wbg", subgroup := "wh_bl_gap"]
seda_long[variable %like% "whg", subgroup := "wh_hs_gap"]
seda_long[variable %like% "mfg", subgroup := "m_fm_gap"]
seda_long[variable %like% "neg", subgroup := "ecd_not_gap"]
seda_long$variable <- NULL

#standardize by subtracting grade (the value is in grade equivalents, and we're converting it to grade equivalent - grade)
seda_long[measure == "mean_achievement" & !subgroup %like% "gap", value := value - grade]

#cast wide by measure
seda_long <- dcast(seda_long, ... ~ measure, value.var = "value")

#save intermediate, cleaned version
#fwrite(seda_long, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs/seda_gcs_long.csv")

#trim gap vars and ec vars to save space
seda_long <- seda_long[!subgroup %like% "gap"]

#save files separately for plotting
for(c.subgroup in unique(seda_long$subgroup)){
  print(c.subgroup)
  fwrite(seda_long[subgroup==c.subgroup],
                   paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs/seda_gcs_long_", c.subgroup, ".csv"))
}

########################################################################
## Pull in ACS and Supplementary file 1 data
########################################################################

#now acs school district data
key <- "75d545894dbeac65a495e1d0ab7d2ae68513362d"
acs_vars<-load_variables(year = 2013, "acs5", cache = TRUE)
acs_vars<- data.table(acs_vars)
sf1_vars<-load_variables(year = 2010, "sf1", cache = TRUE)
sf1_vars<- data.table(sf1_vars)
# sf3_vars<-load_variables(year = 2000, "sf3", cache = TRUE)
# sf3_vars<- data.table(sf3_vars)

vars <- rbind(sf1_vars, acs_vars)
setnames(vars, "name", "variable")

acs_var_list <- c("B20017B_001", "B20017D_001", "B20017H_001", "B20017I_001", "B08121_001", "B20017C_001")
sf1_var_list <- c( "P003001", "P003002", "P003003", "P003005", "P004003", "P003006")

get_acs_vars <- function(c.fip){
  print(c.fip)
  dt <- data.table(get_acs(geography = "school district (unified)", variables = acs_var_list, year = 2013, state = c.fip, key = key))
}
lapply(unique(seda_long$fips), get_acs_vars) %>% rbindlist(fill = T) -> acs_covs

get_sf1_vars <- function(c.fip){
  print(c.fip)
  dt <- data.table(get_decennial(geography = "school district (unified)", variables = sf1_var_list, year = c(2010), state = c.fip, key = key))
}
lapply(unique(seda_long$fips), get_sf1_vars) %>% rbindlist(fill = T) -> sf1_covs

census_covs <- rbindlist(list(acs_covs, sf1_covs), fill = T)
census_covs[is.na(estimate), estimate := value]
census_covs[,value := NULL]

vars_cb <- data.table(variable = c(acs_var_list, sf1_var_list),
                      measure = c(rep("median_income", 6), rep("population", 6)),
                      race = c("black", "asian", "white", "hispanic","all","native", "all", "white", "black", "asian", "hispanic", "native"))

census_covs <- merge(census_covs, vars_cb, all.x = T, by="variable")
census_covs[,moe := NULL]
census_covs[,variable := NULL]

census_covs_wide <- dcast(census_covs, ... ~ race, value.var = "estimate")
for(c.race in c("all", "asian", "black", "hispanic", "native", "white")){
  census_covs_wide[, paste0("prop_", c.race):= get(c.race)/all]
}



########################################################################
## Now pull in common core data
########################################################################
#load in ccd data
c("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1617_l_2a_11212017.csv",
  "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/ccd_lea_052_1718_l_1a_083118.csv",
  "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_lea_052_1819_l_1a_091019/ccd_lea_052_1819_l_1a_091019.csv") %>% 
  lapply(fread) %>% rbindlist() -> ccd

ccd <- ccd[,.(LEAID, SCHOOL_YEAR,SEX, RACE_ETHNICITY, GRADE, STUDENT_COUNT, TOTAL_INDICATOR, LEA_NAME)]

#Standardize variables
ccd <- ccd[,.(STUDENT_COUNT = mean(STUDENT_COUNT, na.rm = T)), by = .(LEAID, SEX, RACE_ETHNICITY, GRADE, TOTAL_INDICATOR, LEA_NAME)]
ccd[is.nan(STUDENT_COUNT), STUDENT_COUNT := NA]
ccd[, all := mean(STUDENT_COUNT[TOTAL_INDICATOR == "Education Unit Total"], na.rm = T), by = LEAID]
ccd <- ccd[TOTAL_INDICATOR %like% "Category Set A"]
ccd[, race_count := sum(STUDENT_COUNT, na.rm = T), .(RACE_ETHNICITY, LEAID)]
ccd <- ccd[,.(LEAID, RACE_ETHNICITY, LEA_NAME, all, race_count)]
ccd <- unique(ccd)

# rename race bins
ccd[ RACE_ETHNICITY %like% "Asian", race := "asian"]
ccd[RACE_ETHNICITY %like% "Native", race := "native"]
ccd[ RACE_ETHNICITY %like% "Black", race := "black"]
ccd[RACE_ETHNICITY %like% "Hispanic", race := "hispanic"]
ccd[RACE_ETHNICITY %like% "White", race := "white"]


ccd <- ccd[complete.cases(ccd)]
ccd[,RACE_ETHNICITY := NULL]


# replace na values with 0, even though there is likely some suppression of small numbers making NA = <5
ccd <- dcast(ccd, ... ~ race, value.var = "race_count", fun.aggregate = sum)
library(gtools)
ccd <- na.replace(ccd, 0)
for(c.race in c("all", "asian", "black", "hispanic", "native", "white")){
  ccd[, paste0("prop_", c.race):= get(c.race)/all]
}
setnames(ccd, c("LEAID", "LEA_NAME"), c("GEOID", "NAME"))
ccd[, measure := "population"]
ccd[, ccd := 1]

#write ccd data
fwrite(ccd, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv")
census_covs_wide[, ccd := 0]
ccd <- ccd[!GEOID %in% unique(census_covs_wide$GEOID)]
census_covs_wide <- rbind(ccd, census_covs_wide, fill = T)


#write combined file of census and ccd data
fwrite(census_covs_wide, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv")


#save code book for array jobs
seda_codes <- data.table(code = unique(seda_long$subgroup))
fwrite(seda_codes, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/seda_codes.csv")

seda_codes_fips <- expand.grid(code = unique(seda_codes$code), fip = unique(seda_long$fips))
fwrite(seda_codes_fips, "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/seda_codes_fips.csv")

