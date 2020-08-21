########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code fills in missing years of information for each school district
## grade, subgroup, subject combination.
########################################################################


# imputation model
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
#library(udunits2)
#library(itertools, lib.loc = "/home/j/temp/hyork/rlibs")
library(tidycensus, lib.loc = "/home/j/temp/hyork/rlibs")
library(missForest, lib.loc = "/home/j/temp/hyork/rlibs")
library(doParallel)
library(haven)
registerDoParallel(cores=5)
cores <- 5

# pull in environmental variables
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
codes <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/seda_codes_fips.csv")
c.subset <- codes[task_id, code]
c.fip <- codes[task_id, fip]


########################################################################
## Input SEDA data, shapefiles
########################################################################

#read data and subset to the state in question
data <- fread(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs/seda_gcs_long.csv"))
data <- data[fips == c.fip]

#read ideal full list of school dists. This is obviously significantly more than what's included in the data
#this is a simplified shapefile for faster loading. There are missing slivers that make this less ideal for zoomed in views
shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")
# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/2013_Unified_Elementary_SD.shp" %>% 
#   read_sf() -> shp


#coerce the shapefile to what we need. I subset to all school districts that have a high grade of 12, reducing some smaller 
#elementary-only school districts. This was done for simplicity's sake but should probably be redone to be exhaustive and non-overlapping
#though the shapefile is unified districts, there is still a lot going on that causes many shapes to overlap
shp <- shp[as.numeric(shp$STATEFP) == c.fip, ]
shp <- data.table(shp)
shp[, leaidC := as.numeric(as.character(GEOID))]
shp[, leanm := NAME]
shp[, fips := as.numeric(STATEFP)]
shp <- shp[!is.na(leaidC), ]
shp <- shp[shp$HIGRADE == "12",]


#add blank rows to SEDA data for school districts missing from data
dt_fill <- data.table(expand.grid(leaidC = unique(c(unique(shp$leaidC), unique(data$leaidC))),
                                  grade = unique(data$grade),
                                  year = 2009:2016,
                                  subject = unique(data$subject),
                                  subgroup = unique(data$subgroup)))
dt_fill <- merge(dt_fill, shp[,.(leaidC, fips)], by = "leaidC", all.x = T)
dt_fill[is.na(fips), fips := as.numeric(str_sub(leaidC,1, -6))]
dt_fill <- merge(dt_fill, unique(data[,.(fips, stateabb)]), by = "fips", all.x = T)
data <- merge(data, dt_fill, all = T, by = names(dt_fill))

########################################################################
# Pull in school finance covariates, impute where possible
########################################################################

"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs/" %>% 
  list.files(., full.names = T, pattern = "sdf|SDF|Sdf") %>% 
  lapply(., function(x){data.table(read.delim(x))}) %>% 
  rbindlist(fill = T) -> finance_cov

finance_cov <- finance_cov[FIPST == c.fip]
#keep only important variables
finance_cov <- finance_cov[,.(LEAID,Z35, MEMBERSCH,TOTALEXP, YEAR)]
finance_cov[,leaidC := as.numeric(as.character(LEAID))]
finance_cov[, LEAID := NULL]

#standardize the year variable
finance_cov[, year := 2000 + as.numeric(YEAR)]

#expand to include all years, when some years are missing
finance_dt <- data.table(expand.grid(leaidC = unique(c(as.numeric(as.character(data$leaidC)), unique(finance_cov$leaidC))), year = 2009:2016))
finance_cov <- merge(finance_cov, finance_dt, all = T, by = c("leaidC", "year"))
finance_cov[, YEAR := NULL]

#standardize variables, remove NA vals
finance_cov[Z35 %in% c(-2, -1, 0), Z35 := NA]
finance_cov[MEMBERSCH %in% c(-2, -1, 0), MEMBERSCH := NA]
finance_cov[TOTALEXP%in% c(-2, -1, 0), TOTALEXP := NA]

#create a salary per student variable
finance_cov[,salary_per_s := Z35/MEMBERSCH]
finance_cov <- finance_cov[,.(leaidC, year, salary_per_s)]

#remove outlier values. This is a non-robust method and can be improved upon
finance_cov[salary_per_s < 100, salary_per_s := NA]
finance_cov[salary_per_s > quantile(finance_cov$salary_per_s, .95, na.rm = T), salary_per_s := quantile(finance_cov$salary_per_s, .95, na.rm = T)]

#linearly impute missing values
finance_imputr = function(c.leaidC, data){
  print(c.leaidC)
  dt = data[leaidC == c.leaidC]
  if(!all(is.na(dt$salary_per_s))){
    mod <- lm(salary_per_s ~ year, dt)
    dt[is.na(salary_per_s), salary_per_s := predict(mod, newdata = .SD), .SDcols = names(dt)]
  }
  return(dt)
}
finance_cov <- rbindlist(mclapply(unique(finance_cov$leaidC), finance_imputr, data = finance_cov, mc.cores = 4), fill = T)
finance_cov[salary_per_s < 100, salary_per_s := 100]

########################################################################
# Now load demographic variables from the census/ACS
########################################################################

#load census vars
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()

pops <- census_covs_wide[measure == "population"]

incomes <- census_covs_wide[measure == "median_income"]

#subset to only appropriate columns, white or all is used as referent group
if(c.subset %in% c("asian", "black", "hispanic", "native", "all")){
  data <- data[subgroup %in% c(c.subset, "white")]
  #if(c.subset != "native"){
  census_covs_wide_sub <-census_covs_wide[,.SD, .SDcols = c("GEOID", "measure", c.subset, paste0("prop_", c.subset), "white", "prop_white")]
  #}else{
  #census_covs_wide_sub <-census_covs_wide[,.SD, .SDcols = c("GEOID", "measure", "white", "prop_white", "all")]
  #}
}else if (c.subset %in% c("white", "female", "male", "ec_disadv", "non_ec_disadv")){
  data <- data[subgroup %in% c(c.subset, "all")]
  census_covs_wide_sub <-census_covs_wide[,.SD, .SDcols = c("GEOID", "measure", "white", "prop_white", "all")]
}


#create census covs
census_covs_merge <- melt(census_covs_wide_sub,id.vars = c("GEOID", "measure"), variable.factor = F)
census_covs_merge[variable %like% "_", subgroup := tstrsplit(variable, "_", keep = 2)]
census_covs_merge[!variable %like% "_", subgroup := variable]
census_covs_merge[variable %like% "prop", measure := paste0("prop_", measure)]
census_covs_merge$variable <- NULL
census_covs_merge <- dcast(census_covs_merge, GEOID + subgroup ~ measure, value.var = "value", fun.aggregate = function(x){mean(x, na.rm = T)})
census_covs_merge[, leaidC := as.numeric(as.character(GEOID))]
census_covs_merge[, GEOID := NULL]

#merge data on covariates
data_covs <- merge(data,finance_cov, by = c("year", "leaidC"), all.x = T)
data_covs <- merge(data_covs,census_covs_merge, by = c("subgroup", "leaidC"), all.x = T)

#write 
dir.create("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs_with_covs/")
fwrite(data_covs[subgroup == c.subset],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs_with_covs/", c.subset, "_", c.fip,".csv"))

#remove these even though they could be useful for modelling
data_covs <-data_covs[,-c("mean_achievement_se", "sample_size")]

#cleanup
data_covs[is.na(median_income), median_income := NA] 
data_covs[is.na(prop_median_income), prop_median_income := NA] 

#creat earchived dataset for recordkeeping
data_covs_arch <- copy(data_covs)
##################################################
#impute school dists with missing years using two
#step algorithm, also generate holdouts for OOSPV
##################################################

output_list <- list()
for(c.holdout in c(0:6)){
  data_ho <- copy(data_covs_arch)
  data_ho[, seq_id := 1:nrow(data_ho)]
  set.seed(5 + (c.holdout * 20))
  test <- data_ho[subgroup == c.subset & !is.na(mean_achievement)][rbinom(nrow(data_ho[subgroup == c.subset& !is.na(mean_achievement)]),1, .1) == 1]
  test[, c.id2 := paste(leaidC, grade, subject,subgroup,year, sep = "_")]
  test[, c.id2] -> ho_ids1
  data_ho <- data_ho[seq_id %in% test$seq_id, mean_achievement := NA]
  
  if(c.holdout %in% c(1,2,3)){
    data_covs <- copy(data_ho)
  }else{
    data_covs <- copy(data_covs_arch)
  }
  data_covs[, c.id := paste(leaidC, grade, subject,subgroup, sep = "_")]
  data_covs[, c.id2 := paste(leaidC, grade, subject,subgroup,year, sep = "_")]
  
  imputr <- function(c.c.id, data){
    mod <- lm(mean_achievement ~ year, data[c.id == c.c.id])
    data[is.na(mean_achievement), imputed_step_1 := 1]
    data[c.id == c.c.id & is.na(mean_achievement), mean_achievement := predict(mod, newdata = .SD), .SDcols = names(data)]
    return(data[c.id == c.c.id])
  }
  
  missingness <- data_covs[, sum(!is.na(mean_achievement)), by = c.id]
  c.ids_impute <- missingness[V1 >=4 &V1 <8, c.id]
  imputed <- rbindlist(mclapply(c.ids_impute, imputr, data = data_covs, mc.cores = cores), fill = T)
  nonimputed <- data_covs[!c.id %in% unique(imputed$c.id)]
  
  data_covs <-  rbindlist(list(imputed, nonimputed), fill = T)
  
  # #
  # data_covs_copy <- copy(data_covs)
  # data_covs_copy <- data_covs_copy %>% mutate_if(is.character,as.factor) %>% data.table
  # data_covs_copy[, leaidC := as.factor(as.character(leaidC))]
  # data_covs_copy[, year := as.factor(as.character(year))]
  # data_covs_copy[, grade := as.factor(as.character(grade))]
  # 
  # system.time(missForest((data_covs_copy[,-c("leaidC", "leanm")][1:1000])))
  #cast wide to predict c.subgroup
  data_covs[is.nan(prop_median_income), median_income := NA]
  data_covs[is.nan(prop_population), population:= NA]
  data_covs[, prop_population_subset := mean(prop_population[subgroup == c.subset], na.rm = T), by = .(year, leaidC)]
  data_covs[, prop_population_ref := mean(prop_population[subgroup != c.subset], na.rm = T), by = .(year, leaidC)]
  
  data_covs[, prop_median_income_subset := mean(prop_median_income[subgroup == c.subset], na.rm = T), by = .(year, leaidC)]
  data_covs[, prop_median_income_ref := mean(prop_median_income[subgroup != c.subset], na.rm = T), by = .(year, leaidC)]
  data_covs[,leanm := NA]
  
  
  d.data_wide <- dcast(data_covs, leaidC + leanm + fips + stateabb + 
                         prop_median_income_subset +prop_median_income_ref + prop_population_subset +prop_population_ref  +year +
                         salary_per_s ~subgroup + grade + subject, 
                       value.var = "mean_achievement")
  
  avg_diffs <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv")
  avg_diffs <- avg_diffs[as.numeric(str_sub(LEAID, 1, -6)) == c.fip]
  avg_diffs[avg_perc_diff < 0, avg_perc_diff := 0]
  avg_diffs[avg_pop < 10, avg_perc_diff := NA]
  avg_diffs_exp <- data.table(expand.grid(LEAID = unique(d.data_wide$leaidC),
                                          race = unique(data_covs$subgroup),
                                          year = unique(d.data_wide$year),
                                          grade = 10:12))
  avg_diffs <- merge(avg_diffs, avg_diffs_exp, all = T)
  avg_diffs <- avg_diffs[LEAID %in% unique(d.data_wide$leaidC)]
  avg_diffs[avg_perc_diff > 4, avg_perc_diff := NA]
  avg_diffs[avg_perc_diff > 2 & avg_pop < 50, avg_perc_diff := NA]
  avg_diffs[unadj_avg_perc_diff < 0, avg_perc_diff := NA]
  avg_diffs[avg_perc_diff < .5 & avg_pop < 50, avg_perc_diff := NA]
  avg_diffs[avg_perc_diff < .1 & avg_pop < 200, avg_perc_diff := NA]
  
  avg_diffs[unadj_avg_perc_diff > .8 & unadj_avg_perc_diff < 1.2 & (avg_perc_diff <= .8 | avg_perc_diff >= 1.2), avg_perc_diff := NA]
  
  avg_diffs_arch <- copy(avg_diffs)
  if(c.holdout %in% 4:6){
    set.seed(5 + (c.holdout*20))
    avg_diffs[,seq_id := 1:nrow(avg_diffs)]
    test2 <- avg_diffs[race == c.subset & !is.na(avg_perc_diff)][rbinom(nrow(avg_diffs[race == c.subset& !is.na(avg_perc_diff)]),1, .1) == 1]
    avg_diffs[seq_id %in% test2$seq_id, avg_perc_diff := NA]
    test2[, c.id3 := paste(LEAID, grade,race,year, sep = "_")]
    test2[,c.id3] -> ho_ids2
  }
  
  avg_diffs_wide <- dcast(avg_diffs[race %in% unique(data_covs$subgroup) & grade %in% 10:12], LEAID + year ~ race + grade, value.var = "avg_perc_diff")
  setnames(avg_diffs_wide, c("LEAID"), c("leaidC"))
  names(avg_diffs_wide)[!names(avg_diffs_wide) %in% c("leaidC", "year")] <- paste0(names(avg_diffs_wide)[!names(avg_diffs_wide) %in% c("leaidC", "year")], "_retent")
  
  d.data_wide <- merge(d.data_wide, avg_diffs_wide, by = c("leaidC", "year"))
  
  forestr <- function(c.fip, c.data_wide){
    print(c.fip)
    data_wide <- c.data_wide %>% mutate_if(is.character,as.factor) %>% data.table
    data_wide[, year := as.factor(as.character(year))]
    data_wide <- data_wide[fips == c.fip]
    data_wide <- data_wide[,.SD, .SDcols = c("leaidC", "leanm", "fips", "stateabb", "year", 
                                             names(data_wide)[!names(data_wide) %in% c("leaidC", "leanm", "fips", "stateabb", "year")])]
    
    imp_out<- missForest((data_wide[,-(1:2)]), parallelize = "no", ntree = 100, maxiter = 10)
    output <- cbind(data_wide[,1:2], imp_out$ximp)
    id_var_list <-c("leaidC", "leanm", "fips", "stateabb", "year")
    output <- melt(output[,.SD,.SDcols = c(id_var_list, names(output)[names(output) %like% "math|ela|retent"])], id.vars = id_var_list)
    output[,c("subgroup", "grade", "subject") := tstrsplit(variable, "_", keep = c(1,2,3), type.convert = T)]
    output[,variable := NULL]
    setnames(output, "value", "mean_achievement")
    return(output)
  }
  output<- mclapply(unique(data_covs$fips), forestr, c.data_wide = d.data_wide, mc.cores = cores) %>% rbindlist(fill = T)
  output[, holdout := c.holdout]
  
  output[, c.id2 := paste(leaidC, grade, subject,subgroup,year, sep = "_")]
  data_covs_arch[, c.id2 := paste(leaidC, grade, subject,subgroup,year, sep = "_")]
  output[c.id2 %in% data_covs_arch[!is.na(mean_achievement), c.id2] & subject != "retent", imputed_step_1 := 0]
  output[c.id2 %in% data_covs_arch[!is.na(mean_achievement), c.id2] & subject != "retent", imputed_step_2 := 0]
  output[is.na(imputed_step_1) & subject != "retent", imputed_step_1 := 1]
  output[is.na(imputed_step_2) & subject != "retent", imputed_step_2 := 1]
  
  
  avg_diffs <- copy(avg_diffs_arch)
  avg_diffs[, c.id3 := paste(LEAID, grade,race,year, sep = "_")]
  output[, c.id3 := paste(leaidC, grade, subgroup,year, sep = "_")]
  output[c.id3 %in% avg_diffs[!is.na(avg_perc_diff), c.id3] & subject == "retent", imputed_step_1 := 0]
  output[c.id3 %in% avg_diffs[!is.na(avg_perc_diff), c.id3]& subject == "retent", imputed_step_2 := 0]
  output[is.na(imputed_step_1) & subject == "retent", imputed_step_1 := 1]
  output[is.na(imputed_step_2) & subject == "retent", imputed_step_2 := 1]
  
  output[, data_ho := 0]
  if(c.holdout %in% 1:3){
    output[c.id2 %in% ho_ids1, data_ho := 1]
  }
  if(c.holdout %in% 4:6){
    output[c.id3 %in% ho_ids2, data_ho := 1]
  }
  
  
  output_list[[c.holdout + 1]] <- output
  
}

rbindlist(output_list) -> output
###########################33




dir.create("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data/")
fwrite(output[subgroup == c.subset & subject != "retent" & holdout == 0],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data/", c.subset,"_", c.fip, ".csv"))

dir.create("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent/")
fwrite(output[subgroup == c.subset & subject == "retent" & holdout %in% 0],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent/", c.subset,"_", c.fip, ".csv"))

dir.create("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent_holdout/")
fwrite(output[subgroup == c.subset & subject == "retent" & holdout %in% 4:6 ],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent_holdout/", c.subset,"_", c.fip, ".csv"))

fwrite(output[subgroup == c.subset & subject != "retent" & holdout %in% 1:3],
       paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_holdouts/", c.subset,"_", c.fip, ".csv"))



