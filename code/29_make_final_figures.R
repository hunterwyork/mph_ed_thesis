########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code takes all outputs and makes final, paper ready figures.
## It also has numberplugging code.
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
library(scales)
library(Hmisc)
library(cowplot)
library(stargazer, lib.loc = "/home/j/temp/hyork/rlibs")
cores <- 5
#################################################
### Make scatters of covariates and data
#################################################

"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs_with_covs/" %>% 
  list.files(., full.names = T) %>%
  mclapply(fread, mc.cores = cores) %>% rbindlist(., fill = T) -> inputs_with_covs

#make unique by schooldistrict-year-subgroup
unique_covs <- inputs_with_covs[,.(mean_achievement = mean(mean_achievement, na.rm = T)),
                                by = .(subgroup, leaidC, year, fips, stateabb, leanm, salary_per_s, median_income, population, prop_median_income, prop_population)]

#take a sample for quicker plotting
unique_covs_sample <- data.table(sample_n(unique_covs, 100000))

#now plot
pdf("/home/j/temp/hyork/scatters_05142020.pdf", height = 10, width = 12)
ggplot(unique_covs_sample) + 
  geom_hex(aes(x = salary_per_s, y = mean_achievement), bins = 50, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_smooth(aes(x = salary_per_s, y = mean_achievement), method = "lm")+
  facet_wrap(~subgroup) + theme_bw() + ggtitle("Teacher Salary per Student")
ggplot(unique_covs_sample[subgroup %in% c("white", "hispanic", "black", "asian", "all")]) + 
  geom_hex(aes(x = log(median_income), y = mean_achievement), bins = 50, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_smooth(aes(x = log(median_income), y = mean_achievement), method = "lm")+
  facet_wrap(~subgroup) + theme_bw()+ ggtitle("Median Income")
ggplot(unique_covs_sample[subgroup %in% c("white", "hispanic", "black", "asian", "all")]) + 
  geom_hex(aes(x = log(population), y = mean_achievement), bins = 50, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_smooth(aes(x = log(population), y = mean_achievement), method = "lm")+
  facet_wrap(~subgroup) + theme_bw()+ ggtitle("Population")
ggplot(unique_covs_sample[subgroup %in% c("white", "hispanic", "black", "asian")]) + 
  geom_hex(aes(x = prop_population, y = mean_achievement), bins = 50, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_smooth(aes(x = prop_population, y = mean_achievement), method = "lm")+
  facet_wrap(~subgroup) + theme_bw()+ ggtitle("Proportion of the population")
ggplot(unique_covs_sample[subgroup %in% c("white", "hispanic", "black", "asian")]) + 
  geom_hex(aes(x = prop_median_income, y = mean_achievement), bins = 40, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07") +
  geom_smooth(aes(x = prop_median_income, y = mean_achievement), method = "lm")+
  facet_wrap(~subgroup) + theme_bw()+ ggtitle("Ratio of Subgroup-Specific Median Income to Overall Median Income")

dev.off()

unique_covs_sample %>% 
  dcast(., leaidC + year + fips + stateabb + leanm~ subgroup, 
        value.var = "mean_achievement", fun.aggregate = function(x){mean(x, na.rm = T)}) -> cast_covs_sample

ggplot(cast_covs_sample) + 
  geom_hex(aes(x = asian, y = all))

#################################################
### Scatter using predicted data
#################################################
"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs_with_covs/" %>% 
  list.files(., full.names = T) %>%
  mclapply(fread, mc.cores = cores) %>% rbindlist(., fill = T) -> inputs_with_covs


covariates <- inputs_with_covs[,.(subgroup, leaidC, year, fips, stateabb, salary_per_s, median_income, prop_population, prop_median_income)] %>% unique()


lasy <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/lasy_aggregations/all.csv")
#lasy <- lasy[subgroup == "all"]
lasy[imputed_step_1 + imputed_step_2 < 1, imputed := "Some or Complete Achievement Data"]
lasy[imputed_step_1 + imputed_step_2 == 1, imputed := "Missing Achievement Data"]

#load orig data on retention
"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv" %>% 
  fread() -> retent_data
retent_data[!is.na(avg_perc_diff), missing := 0]
retent_data[is.na(avg_perc_diff), missing := 1]
retent_missing <- retent_data[,.(retent_missingness = mean(missing)), by = .(LEAID, race, year)]

lasy <- merge(lasy, retent_missing, by.x = c("subgroup", "leaidC", "year"), by.y = c("race", "LEAID", "year"), all.x = T)
lasy[is.na(retent_missingness), retent_missingness := 1]
lasy[retent_missingness == 1, retent_missing := "Missing Retention Data"]
lasy[retent_missingness < 1, retent_missing := "Some or Complete Retention Data"]
###add masking

#census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% fread()

pops <- census_covs_wide[measure == "population"]
for(c.race in c("asian", "black", "hispanic", "white", "native")){
  pops[(get(c.race) < 1500 | get(paste0("prop_", c.race)) < .05) & ccd == 0, (paste0("mask_", c.race)) := 1]
  pops[(get(c.race) < (1500/6.24) | get(paste0("prop_", c.race)) < .05) & ccd == 1, (paste0("mask_", c.race)) := 1]
  pops[is.na(get(paste0("mask_", c.race))), (paste0("mask_", c.race)) := 0]
}
for(c.race in c("black", "white", "hispanic", 'asian', 'native')){
  lasy[!leaidC %in%pops[get(paste0("mask_", c.race)) == 0, GEOID] & subgroup == c.race, mask := 1]
}
lasy[is.na(mask), mask := 0]

lasy_covs <- merge(lasy, covariates, by = c("subgroup", "leaidC", "year", "stateabb", "fips"))

#now also pull in other covs
seda_covs <- data.table(read_dta("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/SEDA_cov_geodist_long_v30 (1).dta"))
seda_covs <- seda_covs[,.SD, .SDcols = c("leaid", "year", "grade", "fips", "stateabb", names(seda_covs)[names(seda_covs) %like% "^ses|^poverty|^single_mom|^unemp|^snap"])]
seda_covs_long <- melt(seda_covs, id.vars = c("leaid", "year", "grade", "fips", "stateabb"))
seda_covs_long[, indicator := str_sub(variable, 1, -4)]
seda_covs_long[, subgroup := str_sub(variable, -3, -1)]
seda_covs_long <- seda_covs_long[indicator %in% c("ses", "poverty", "single_mom", "unemp", "snap")]
seda_covs_wide <- dcast(seda_covs_long, leaid + year + grade + fips + stateabb + subgroup ~ indicator, value.var = "value")
race_map <- data.table(subgroup = c("all", "blk", "hsp", "wht"), race = c("all", "black", "hispanic", "white"))
seda_covs_wide <- merge(seda_covs_wide, race_map, by = c("subgroup"))
seda_covs_wide <- seda_covs_wide[, grade := NULL] %>% unique()
lasy_covs <- merge(lasy_covs, seda_covs_wide, by.x = c('leaidC', 'year', 'subgroup', 'stateabb', 'fips'), by.y = c("leaid", "year", "race", 'stateabb', 'fips'), all.x = T)

plot_data <- lasy_covs[mask == 0 & subgroup != "all"]
plot_data[subgroup == "black", Subgroup := "Black"]
plot_data[subgroup == "hispanic", Subgroup := "Hispanic"]
plot_data[subgroup == "white", Subgroup := "White"]
plot_data[subgroup == "asian", Subgroup := "Asian"]
plot_data[subgroup == "native", Subgroup := "Native"]
plot_data[, log_med_income := log(median_income)]

graph_dat <- data.table()
for(c.race in unique(plot_data$subgroup)){
  if(c.race %in% c("asian", "native")){
    graphy <- sample_n(plot_data[subgroup == c.race & complete.cases(plot_data[,.(salary_per_s, median_income)])], 250)
  }else{
    graphy <- sample_n(plot_data[subgroup == c.race & complete.cases(plot_data)], 250)
  }
  graph_dat <- rbind(graph_dat, graphy, fill = T)
}

library(ggpubr)
# Grouped Scatter plot with marginal density plots
gg1 <- ggscatterhist(
  graph_dat[subgroup != "asian" & subgroup != "native"], x = "single_mom", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5)[c(2,3,5)],
  xlab = "% of Housholds Led By a Single Mother", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg2 <- ggscatterhist(
  graph_dat, x = "median_income", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5),
  xlab = "Median Income", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
 legend = "none")

gg3 <- ggscatterhist(
  graph_dat[subgroup != "asian"& subgroup != "native"], x = "unemp", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5)[c(2,3,5)],
  xlab = "Unemployment Rate", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg4 <- ggscatterhist(
  graph_dat[subgroup != "asian"& subgroup != "native"], x = "poverty", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5)[c(2,3,5)],
  xlab = "% of Households in Poverty", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg5 <- ggscatterhist(
  graph_dat, x = "salary_per_s", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5),
  xlab = "Avg. Teacher Salary per Student (USD)", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg6 <- ggscatterhist(
  graph_dat[subgroup != "asian"& subgroup != "native"], x = "ses", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5)[c(2,3,5)],
  xlab = "Socioeconomic Status", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)

gglegend <- ggscatterhist(
  graph_dat, x = "salary_per_s", y = "lasy",
  color = "Subgroup", size = 3, alpha = 1, shape = 1,
  palette = brewer_pal(palette = "Set1", direction = -1)(5),
  xlab = "Avg. Teacher Salary per Student (USD)", ylab = "LAYS",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "bottom"
)

pdf("/home/j/temp/hyork/scatters060620203.pdf", width = 10, height = 7)
grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6, nrow = 2)
print(gglegend)
dev.off()



plot_data[year %in% c(2010, 2016)] %>%
  melt(id.vars = c("leaidC", "year", "subgroup", "stateabb", "Subgroup"),
       measure.vars = c("lasy", "salary_per_s", "median_income", "poverty", "ses", "single_mom", "unemp")) %>% 
  dcast(leaidC + subgroup + stateabb + Subgroup + variable ~ year) %>% .[, perc_change := (`2016` - `2010`)] %>% 
  dcast(leaidC + subgroup + stateabb + Subgroup ~ variable, value.var = "perc_change") -> plot_data_changes



graph_dat_changes <- data.table()
for(c.race in unique(plot_data_changes$subgroup)){
  if(c.race %in% c("asian", "native")){
    graphy <- sample_n(plot_data_changes[subgroup == c.race & complete.cases(plot_data_changes[,.(salary_per_s, median_income)])], 1000, replace = T)
  }else{
    graphy <- sample_n(plot_data_changes[subgroup == c.race & complete.cases(plot_data_changes)], 1000, replact = T)
  }
  graph_dat_changes <- rbind(graph_dat_changes, graphy, fill = T)
}

gg1 <- ggscatterhist(
  graph_dat_changes[subgroup != "asian"], x = "single_mom", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "% of Housholds Led By a Single Mother", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg2 <- ggscatterhist(
  graph_dat_changes[subgroup != "asian"], x = "median_income", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "Median Income", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none")

gg3 <- ggscatterhist(
  graph_dat_changes[subgroup != "asian"], x = "unemp", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "Unemployment Rate", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg4 <- ggscatterhist(
  graph_dat_changes[subgroup != "asian"], x = "poverty", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "% of Households in Poverty", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg5 <- ggscatterhist(
  graph_dat_changes, x = "salary_per_s", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#4C9900","#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "Avg. Teacher Salary per Student (USD)", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)
gg6 <- ggscatterhist(
  graph_dat_changes[subgroup != "asian"], x = "ses", y = "lasy",
  color = "Subgroup", size = 1, alpha = .5, shape = 1,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  xlab = "Socioeconomic Status", ylab = "LASYs",
  margin.params = list(fill = "Subgroup", color = "black", size = 0.2),
  legend = "none"
)

grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6, nrow = 2)







ggplot() + 
  geom_point(aes(x = median_income, y = lasy, color = subgroup), alpha = .2, size = .7) + 
  theme_bw()

ggplot(graph_dat) + 
  geom_point(aes(x = single_mom, y = lasy, color = subgroup))

ggplot(graph_dat) + 
  geom_point(aes(x = ses, y = lasy, color = subgroup))

ggplot(graph_dat) + 
  geom_point(aes(x = unemp, y = lasy, color = subgroup))

#################################################
### Table 1, missingness
#################################################
paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs_with_covs/") %>% 
  list.files(full.names = T) %>% mclapply(fread, mc.cores = cores) %>% 
  rbindlist(., fill = T) -> data_with_covs 

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
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72"),]
#shp <- shp[!is.na(shp$UNSDLEA),]
shp <- shp[shp$ALAND > 0,]
shp <- shp[shp$HIGRADE == "12",]
shp <- shp[shp$LOGRADE %in% c("PK", "KG"),]

#only keep the good data

avg_diffs <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv")
avg_diffs[, race := as.character(race)]
data_with_covs <- merge(data_with_covs, avg_diffs, 
                        by.x = c("subgroup", "leaidC", "year", "grade"),
                        by.y = c("race", "LEAID", "year", "grade"), all = T)

shp$GEOID <- as.numeric(shp$GEOID)
data_with_covs <- data_with_covs[leaidC %in% unique(shp$GEOID)]

data_with_covs <- merge(data_with_covs, shp[, c("GEOID", "NAME")], by.x = "leaidC", by.y = "GEOID")

census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()
#census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% fread()
pops <- census_covs_wide[measure == "population"]
pops_long <- melt(pops[,.(GEOID, all, asian, black, hispanic, native, white)], id.var = "GEOID")
setnames(pops_long, "value", "population")
unique(pops_long) %>% .[,.(population = mean(population)), by = .(GEOID, variable)] -> pops_long
data_with_covs[, population := NULL]
data_with_covs <- merge(data_with_covs,pops_long , by.x = c("leaidC", "subgroup"), by.y = c("GEOID", "variable"), all.x = T)

for(c.race in c("asian", "black", "hispanic", "white", "native")){
  pops[(get(c.race) < 1500 | get(paste0("prop_", c.race)) < .05) & ccd == 0, (paste0("mask_", c.race)) := 1]
  pops[(get(c.race) < (1500/6.24) | get(paste0("prop_", c.race)) < .05) & ccd == 1, (paste0("mask_", c.race)) := 1]
  pops[is.na(get(paste0("mask_", c.race))), (paste0("mask_", c.race)) := 0]
}

tabler <- function(c.subgroup, c.data, c.pop){
  data <- c.data[subgroup == c.subgroup]
  if(c.subgroup != "all" & c.subgroup != "white"){
    data <- data[leaidC %in% c.pop[get(paste0("mask_", c.subgroup)) == 0, GEOID]]
  }
  if(c.subgroup == "white"){
    data <- data[!leaidC %in% c.pop[get(paste0("mask_", c.subgroup)) == 1, GEOID]]
  }
  table <- data[,.(population = length(population[!is.na(population)])/length(population),
                   median_income = length(median_income[!is.na(median_income)])/length(median_income),
                   mean_achievement = length(mean_achievement[!is.na(mean_achievement)])/length(mean_achievement),
                   salary_per_s = length(salary_per_s[!is.na(salary_per_s)])/length(salary_per_s),
                   retention =  length(avg_perc_diff[!is.na(avg_perc_diff)])/length(avg_perc_diff),
                   total_leaids = length(unique(leaidC)))]
  table[, subgroup := c.subgroup]
  return(table)
}

lapply( c("asian", "black", "hispanic","native", "white", "all"),tabler, c.data = data_with_covs, c.pop = pops) %>% rbindlist() -> tables

tables <- tables[,.SD, .SDcols = c("subgroup", "population", "median_income", "mean_achievement", "salary_per_s","retention", "total_leaids")]
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
tables[,names(tables)[-c(1, 7)] := lapply(.SD, percent), .SDcols = names(tables)[-c(1, 7)]]
tables[,subgroup := Hmisc::capitalize(subgroup)]
tables[, total_leaids := formatC(total_leaids, big.mark = ",")]
setnames(tables, c("&nbspRace/Ethnicity&nbsp", "&nbspPopulation&nbsp", "&nbspMedian Income&nbsp", "&nbspTesting Outcomes&nbsp", "&nbspTeacher Salaries&nbsp","High School Retention&nbsp", "&nbspTotal Districts"))
htmlTable::htmlTable(tables, summary = F, type = "&nbsphtml&nbsp", out = "/home/j/temp/hyork/table.html", rnames = F,
                     caption = "Table 1. Data Availability by School District and Race/Ethnicity")


############################
## Table beep
###########################
table2 <- data.table("Prediction Subgroup" = c("White", "Black", "Asian", "Hispanic"), "Referent Subgroup"= c("All", "White", "All", "All"))
stargazer(table2, summary = F, type = "html", out = "/home/j/temp/hyork/table2.html")


############################
## Create LAYS using only complete data
############################
data <- fread(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/inputs/seda_gcs_long.csv"))
data <- data[subgroup %in% c("all", "asian", "black", "hispanic", "native", "white")]
data <- data[!is.na(mean_achievement)]
data[, completeness := .N/96, by = .(leaidC, leanm, fips, subgroup)]
data <- data[completeness == 1]

#collapse to one learning number 
mean_learning <- data[,.(mean_achievement = mean(mean_achievement)), by = .(leaidC, leanm, fips, subgroup)]

#load dropout rates
avg_diffs <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv")
retent_12 <- avg_diffs[grade %in% 10:12 & avg_perc_diff >= 0 & avg_perc_diff <= 1 & min_year == 2017 & max_year == 2018,
                       .(retention = prod(avg_perc_diff, na.rm = T),
                         avg_pop = mean(avg_pop),
                         grade = 12), by= .(LEAID, race)]
retent_11 <- avg_diffs[grade %in% 10:11 & avg_perc_diff >= 0 & avg_perc_diff <= 1 & min_year == 2017 & max_year == 2018,
                       .(retention = prod(avg_perc_diff, na.rm = T),
                         avg_pop = mean(avg_pop),
                         grade = 11), by= .(LEAID, race)]
retent_10 <- avg_diffs[grade %in% 10 & avg_perc_diff >= 0 & avg_perc_diff <= 1 & min_year == 2017 & max_year == 2018,
                       .(retention = prod(avg_perc_diff, na.rm = T),
                         avg_pop = mean(avg_pop),
                         grade = 10), by= .(LEAID, race)]

retent <- rbindlist(list(retent_12, retent_11, retent_10))
retent <- retent[avg_pop >= 30]
retent[, count := .N, by = .(LEAID, race)]
retent <- retent[count == 3]

retent_wide <- dcast(retent, LEAID + race~grade, value.var = "retention")
retent_wide[, yos := 10 + `10` + `11` + `12`]
retent_wide <- retent_wide[,.(LEAID, race, yos)]


#merge mean learning and retention
lasy <- merge(retent_wide, mean_learning, by.x = c("LEAID", "race"), by.y = c("leaidC", "subgroup"))
lasy[, lasy := yos + mean_achievement]
lasy[mean_achievement <= 0, lasy_cap := yos + mean_achievement]
lasy[mean_achievement > 0, lasy_cap := yos]
lasy[, leaidC := LEAID]
lasy_melt <- melt(lasy, id.vars = c("LEAID", "race", "leaidC", "leanm", "fips"))
#graph 
lasy_melt_shp <- merge(lasy_melt, shp, by = "leaidC", all = T)

plottr <- function(c.var){
  gg1 <- ggplot(lasy_melt_shp[race == "all" & variable == c.var]) + 
    geom_sf(aes(fill = value, geometry = geometry), lwd = 0) + 
    scale_fill_viridis_c()+
    theme_bw()
  return(gg1)
}

grob_list <- lapply(unique(lasy_melt$variable), plottr)

grid.arrange(grobs = grob_list, nrow = 2)




##############################################################
## Make figure 1
#############################################################

lasy <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/lasy_aggregations/all.csv")
#lasy <- lasy[subgroup == "all"]
lasy[imputed_step_1 + imputed_step_2 < 1, imputed := "Some or Complete Achievement Data"]
lasy[imputed_step_1 + imputed_step_2 == 1, imputed := "Missing Achievement Data"]

#load orig data on retention
"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv" %>% 
  fread() -> retent_data
retent_data[!is.na(avg_perc_diff), missing := 0]
retent_data[is.na(avg_perc_diff), missing := 1]
retent_missing <- retent_data[,.(retent_missingness = mean(missing)), by = .(LEAID, race, year)]

lasy <- merge(lasy, retent_missing, by.x = c("subgroup", "leaidC", "year"), by.y = c("race", "LEAID", "year"), all.x = T)
lasy[is.na(retent_missingness), retent_missingness := 1]
lasy[retent_missingness == 1, retent_missing := "Missing Retention Data"]
lasy[retent_missingness < 1, retent_missing := "Some or Complete Retention Data"]

#read ideal full list of school dists
shp <- readRDS("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/schooldistrict_sy1819_tl19_simp.rds")
# "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/2013_Unified_Elementary_SD.shp" %>% 
#   read_sf() -> shp

#add blank rows for school districts missing from data
shp$fips <- as.numeric(shp$STATEFP)
#shp$leaidC <- as.numeric(str_sub(shp$GEOID), 3, -1)
shp$leaidC <- as.numeric((shp$GEOID))

shp <- st_transform(shp, crs ="+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100")

#get rid of AK, HI, and PR
shp <- shp[shp$STATEFP <= 56 & !shp$STATEFP %in% c("72", "02", "15"),]
#shp <- shp[!is.na(shp$UNSDLEA),]
shp <- shp[shp$ALAND > 0,]
shp <- shp[shp$HIGRADE == "12",]
#shp <- shp[shp$LOGRADE %in% c("KG", "PK"),]
#only keep the good data
lasy <- lasy[leaidC %in% as.numeric(shp$GEOID)]

states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)
states_shp <- states_shp[states_shp$STATEFP %in% unique(shp$STATEFP),]


#retrieve masking


###add masking

census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()
pops <- census_covs_wide[measure == "population"]


for(c.race in c("asian", "black", "hispanic", "white", "native")){
  pops[(get(c.race) < 1500 | get(paste0("prop_", c.race)) < .05) & ccd == 0, (paste0("mask_", c.race)) := 1]
  pops[(get(c.race) < (1500/6.24) | get(paste0("prop_", c.race)) < .05) & ccd == 1, (paste0("mask_", c.race)) := 1]
  pops[is.na(get(paste0("mask_", c.race))), (paste0("mask_", c.race)) := 0]
}


for(c.race in c("black", "white", "hispanic", 'asian', 'native')){
  lasy[!leaidC %in%pops[get(paste0("mask_", c.race)) == 0, GEOID] & subgroup == c.race, mask := 1]
}
lasy[is.na(mask), mask := 0]


####
lasy_shp <- merge(lasy, shp, by = "leaidC")
lasy_avg <- lasy[,.(mean_achievement = mean(mean_achievement),
                    yos = mean(yos),
                    lasy = mean(lasy),
                    retent_missingness = mean(retent_missingness),
                    imputed_step_2 = mean(imputed_step_2),
                    imputed_step_1 = mean(imputed_step_1)), by = .(leaidC, subgroup, mask, stateabb)]

lasy_avg[retent_missingness == 1, retent_missing := "Missing Retention Data"]
lasy_avg[retent_missingness < 1, retent_missing := "Some or Complete Retention Data"]
lasy_avg[imputed_step_1 + imputed_step_2 < 1, imputed := "Some or Complete Achievement Data"]
lasy_avg[imputed_step_1 + imputed_step_2 == 1, imputed := "Missing Achievement Data"]

lasy_avg_shp <- merge(lasy_avg, shp, by = "leaidC")


gg1 <- ggplot(lasy_avg_shp[ subgroup == "all" ]) + 
  geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = .1) +
  scale_fill_viridis_c(breaks = seq(-4,4,2), limits = c(-4,4), option = "magma") +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  #scale_color_manual(values = c("green", "gray50"))+
  ggtitle("Mean Achievement") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Grade Levels Above/Below National Average") + 
  theme(legend.position = "bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(color = "")

gg1.5 <-  ggplot(lasy_avg_shp[subgroup == "all" ]) + 
  geom_sf(aes(fill = imputed, geometry = geometry), lwd = .01) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  scale_fill_manual(values = c("red", NA))+
  ggtitle("Missing Data") + theme_void() +
  theme(legend.position = "none", plot.title = element_text(size=10))

gg_inset_map1 = ggdraw() +
  draw_plot(gg1) +
  draw_plot(gg1.5, x = 0.06, y = 0.13, width = .3, height = .24)

gg2 <- ggplot(lasy_shp[year == 2016  & subgroup == "all" ]) + 
  geom_sf(aes(fill = yos, geometry = geometry), lwd = .05) +
  scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  ggtitle("Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "Years of Schooling") + 
  theme(legend.position = "bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(color = "")

gg2.5 <-  ggplot(lasy_avg_shp[subgroup == "all" ]) + 
  geom_sf(aes(fill = retent_missing, geometry = geometry), lwd = .01) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  scale_fill_manual(values = c("red", NA))+
  ggtitle("Missing Data") + theme_void() +
  theme(legend.position = "none", plot.title = element_text(size=10))

gg_inset_map2 = ggdraw() +
  draw_plot(gg2) +
  draw_plot(gg2.5, x = 0.06, y = 0.13, width = .3, height = .24)

gg3 <- ggplot(lasy_avg_shp[subgroup == "all"]) + 
  geom_sf(aes(fill = lasy_capped, geometry = geometry),color = "gray50", lwd = .1) +
  scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  scale_color_manual(values = c("red", "gray50"))+
  ggtitle("Learning-Adjusted Years of Schooling (Capped at 13)") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "LAYS (Capped)") + 
  theme(legend.position = "bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(color = "")

gg4 <- ggplot(lasy_avg_shp[subgroup == "all"]) + 
  geom_sf(aes(fill = lasy, geometry = geometry),color = "gray50", lwd = .1) +
  scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  scale_color_manual(values = c("red", "gray50"))+
  ggtitle("Learning-Adjusted Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "LAYS (Not Capped)") + 
  theme(legend.position = "bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(color = "")

pdf("/home/j/temp/hyork/fig_1_05192020.pdf", height = 20, width = 10)
grid.arrange(gg_inset_map1,gg_inset_map2,gg4, nrow = 3, top = "Average Across Years 2009-2016")
dev.off()

#####
## now by year
#####
for(c.year in 2009:2016){
  gg1 <- ggplot(lasy_shp[year == c.year & subgroup == "all" ]) +
    geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = .1) +
    scale_fill_viridis_c(breaks = seq(-4,4,2), limits = c(-4,4), option = "magma") +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    #scale_color_manual(values = c("green", "gray50"))+
    ggtitle("Mean Achievement") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "Grade Levels Above/Below National Average") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg1.5 <-  ggplot(lasy_shp[year == c.year & subgroup == "all" ]) +
    geom_sf(aes(fill = imputed, geometry = geometry), lwd = .01) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_fill_manual(values = c("red", NA))+
    ggtitle("Missing Data") + theme_void() +
    theme(legend.position = "none", plot.title = element_text(size=10))
  
  gg_inset_map1 = ggdraw() +
    draw_plot(gg1) +
    draw_plot(gg1.5, x = 0.09, y = 0.14, width = .3, height = .24)
  
  gg2 <- ggplot(lasy_shp[year == c.year  & subgroup == "all" ]) +
    geom_sf(aes(fill = yos, geometry = geometry), lwd = .05) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    ggtitle("Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "Years of Schooling") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg2.5 <-  ggplot(lasy_shp[year == c.year & subgroup == "all" ]) +
    geom_sf(aes(fill = retent_missing, geometry = geometry), lwd = .01) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_fill_manual(values = c("red", NA))+
    ggtitle("Missing Data") + theme_void() +
    theme(legend.position = "none", plot.title = element_text(size=10))
  
  gg_inset_map2 = ggdraw() +
    draw_plot(gg2) +
    draw_plot(gg2.5, x = 0.09, y = 0.14, width = .3, height = .24)
  
  gg3 <- ggplot(lasy_shp[year == c.year & subgroup == "all"]) +
    geom_sf(aes(fill = lasy_capped, geometry = geometry),color = "gray50", lwd = .1) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_color_manual(values = c("red", "gray50"))+
    ggtitle("Learning-Adjusted Years of Schooling (Capped at 13)") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "LAYS (Capped)") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg4 <- ggplot(lasy_shp[year == c.year & subgroup == "all"]) + 
    geom_sf(aes(fill = lasy, geometry = geometry),color = "gray50", lwd = .1) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_color_manual(values = c("red", "gray50"))+
    ggtitle("Learning-Adjusted Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "LAYS (Not Capped)") + 
    theme(legend.position = "bottom", 
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  jpeg(paste0("/home/j/temp/hyork/fig_1_05192020", c.year, ".jpeg"), height = 2000, width = 1000)
  grid.arrange(gg_inset_map1,gg_inset_map2,gg4, nrow = 3, top = paste0("School Year ",c.year,"-", (c.year + 1)))
  dev.off()
  
  jpeg(paste0("/home/j/temp/hyork/fig_1_05192020_just_lasy", c.year, ".jpg"), height = 700, width = 1000)
  grid.arrange(gg4, nrow = 1, top = paste0("School Year ",c.year,"-", (c.year + 1)))
  dev.off()
}


#####
## now plot lasy by subgroup
#####
for(c.subgroup in c("white", 'black', 'hispanic', 'asian')){
  gg1 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = mean_achievement, geometry = geometry), lwd = .1) +
    scale_fill_viridis_c(breaks = seq(-4,4,2), limits = c(-4,4), option = "magma") +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    #scale_color_manual(values = c("green", "gray50"))+
    ggtitle("Mean Achievement") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "Grade Levels Above/Below National Average") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg1.5 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = imputed, geometry = geometry), lwd = .01) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_fill_manual(values = c("red", NA))+
    ggtitle("Missing Data") + theme_void() +
    theme(legend.position = "none", plot.title = element_text(size=10))
  
  gg_inset_map1 = ggdraw() +
    draw_plot(gg1) +
    draw_plot(gg1.5, x = 0.09, y = 0.14, width = .3, height = .24)
  
  gg2 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = yos, geometry = geometry), lwd = .05) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    ggtitle("Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "Years of Schooling") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg2.5 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = retent_missing, geometry = geometry), lwd = .01) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_fill_manual(values = c("red", NA))+
    ggtitle("Missing Data") + theme_void() +
    theme(legend.position = "none", plot.title = element_text(size=10))
  
  gg_inset_map2 = ggdraw() +
    draw_plot(gg2) +
    draw_plot(gg2.5, x = 0.09, y = 0.14, width = .3, height = .24)
  
  gg3 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = lasy_capped, geometry = geometry),color = "gray50", lwd = .1) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_color_manual(values = c("red", "gray50"))+
    ggtitle("Learning-Adjusted Years of Schooling (Capped at 13)") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "LAYS (Capped)") +
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  gg4 <- ggplot(lasy_shp[year == 2016 & subgroup == c.subgroup & mask == 0]) + 
    geom_sf(aes(fill = lasy, geometry = geometry),color = "gray50", lwd = .1) +
    scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
    geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
    scale_color_manual(values = c("red", "gray50"))+
    ggtitle("Learning-Adjusted Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
    labs(fill = "LAYS (Not Capped)") + 
    theme(legend.position = "bottom", 
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + labs(color = "")
  
  # jpeg(paste0("/home/j/temp/hyork/fig_1_05192020", c.year, ".jpeg"), height = 2000, width = 1000)
  # grid.arrange(gg_inset_map1,gg_inset_map2,gg4, nrow = 3, top = paste0("School Year ",c.year,"-", (c.year + 1)))
  # dev.off()
  
  pdf(paste0("/home/j/temp/hyork/fig_1_05192020", c.subgroup, ".pdf"), height = 20, width = 10)
  grid.arrange(gg_inset_map1,gg_inset_map2,gg4, nrow = 3,  top = paste0("School Year 2016-2017, ", c.subgroup))
  dev.off()
  print(c.subgroup)
}

#just facet lasy by race
gg5 <- ggplot(lasy_shp[year == 2016  & mask == 0 & ! subgroup %in% c("all", "native")]) + 
  geom_sf(aes(fill = lasy, geometry = geometry),color = "gray50", lwd = .1) +
  scale_fill_viridis_c(breaks = c(6,9,12,15,17), limits = c(6,17),values =c(0,(logit(seq(0.001,.999, .001)) + 7)/14, 1)) +
  geom_sf(data = states_shp, aes(geometry = geometry),fill = NA, color = "black", lwd = .05) +
  scale_color_manual(values = c("red", "gray50"))+
  ggtitle("Learning-Adjusted Years of Schooling") + xlab("Longitude") + ylab("Latitude")+ theme_bw() +
  labs(fill = "LAYS (Not Capped)") + 
  facet_wrap(~subgroup) +
  theme(legend.position = "bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + labs(color = "")

pdf(paste0("/home/j/temp/hyork/fig_1_05192020_by_race.pdf"), height = 9, width = 18)
print(gg5)
dev.off()

###########################
#number plug results
###########################

lasy[lasy == min(lasy)]




###########################
#Figure 2
###########################
lasy_long <- melt(lasy, id.vars = c("subgroup", "leaidC", "year", "imputed_step_1", "imputed_step_2", "stateabb", "fips", "mask"), measure.vars = c("lasy", "mean_achievement", "yos"))
lasy_wide <- dcast(lasy_long, leaidC + year + variable~ subgroup, value.var = c("mask", "value"))
states_shp <- data.table(read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp"))
states_shp[,fips := as.numeric(STATEFP)]
lasy_wide[, fips := as.numeric(str_sub(leaidC, 1, -6))]
lasy_wide <- merge(lasy_wide, states_shp[,.(fips, NAME)], by= "fips")

#
lasy_wide[mask_black != 1 & mask_white != 1, black_white_gap := value_black - value_white]
lasy_wide[mask_hispanic != 1 & mask_white != 1, hispanic_white_gap := value_hispanic - value_white]

#
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% fread()
pops <- census_covs_wide[measure == "population"]
pops %>% na.replace(., 0) -> pops
setnames(pops, "NAME", "SD Name")
lasy_wide <- merge(lasy_wide, pops, by.x = "leaidC", by.y = "GEOID")

lasy_sum <- lasy_wide[,.(value_all =weighted.mean(value_all, all,na.rm = T),
                         value_black =weighted.mean(value_black, black,na.rm = T),
                         value_hispanic =weighted.mean(value_hispanic, hispanic,na.rm = T),
                         value_white =weighted.mean(value_white, white,na.rm = T),
                         value_asian =weighted.mean(value_asian, asian,na.rm = T)), by = .(year, fips, NAME, variable)]

lasy_sum[,black_white_gap := value_black - value_white]
lasy_sum[,hispanic_white_gap := value_hispanic - value_white]
setnames(lasy_sum, "variable", "measure")
lasy_sum %>% melt(., id.vars = c("NAME", "fips", "year", "measure")) %>% 
  .[,.(abs_change = value[year == 2016]-value[year == 2009], 
       perc_change =( value[year == 2016]-value[year == 2009])/value[year == 2009], 
       mean_value = mean(value)), by = .(NAME, fips, variable, measure)] -> perc_change

ggplot(lasy_sum, aes(x = year, y = black_white_gap, group = NAME, color = NAME)) + geom_line()
ggplot(perc_change, aes(x = NAME, y = abs_change)) + geom_bar(stat = "identity")+ facet_wrap(~variable) + coord_flip()

perc_change_wide <- dcast(perc_change, NAME + fips  ~ measure + variable, value.var = c('abs_change', 'perc_change', 'mean_value'))
perc_change_wide <- perc_change_wide[,.(NAME,fips, mean_value_lasy_value_all, mean_value_yos_value_all, mean_value_mean_achievement_value_all, perc_change_lasy_value_all, mean_value_lasy_black_white_gap, mean_value_lasy_hispanic_white_gap )]
setnames(perc_change_wide, c("State","fips", "Average LAYS",  "Average YOS","Avg. Learning (Grades Ahead/Behind)", "% Change LAYS 2009-2016", "Avg. Black-White Gap (LAYS)", "Average Hispanic-White Gap (LAYS)"))

perc_change_wide %>%
  melt(., id.vars = c("State", "fips")) %>% .[, value_frac := (value - min(value, na.rm = T))/(max(value, na.rm = T) - min(value, na.rm = T)), 
                                              by = .(variable)] -> plot_data
plot_data[, State := factor(State, levels = plot_data[variable == "Average LAYS"][order(value), State])]

#state sum pops
pops_melt <- melt(pops, id.vars = "GEOID", measure.vars = c("all", "asian", "black", "hispanic", "native"))
pops_melt[, fips := as.numeric(str_sub(GEOID, 1, -6))]
pops_race <- pops_melt[,.(sum_st = sum(value)), by = .(fips, variable)]
pops_race_wide <- dcast(pops_race, fips ~ variable)
for(c.race in c("asian", "black", "hispanic", "native")){
  pops_race_wide[, paste0("prop_", c.race) := get(c.race)/all]
  pops_race_wide[get(paste0("prop_", c.race)) < .1, paste0("mask_", c.race) := 1]
}
na.replace(pops_race_wide, 0) -> pops_race_wide

#merge on data
plot_data <- merge(plot_data, pops_race_wide, by = "fips")
plot_data[variable %like% "Change LAYS", value := value * 100]
for(c.race in c("black", "hispanic")){
  plot_data[tolower(variable) %like% c.race & get(paste0("mask_", c.race)) == 1,value := NA]
  plot_data[tolower(variable) %like% c.race& get(paste0("mask_", c.race)) == 1,value_frac := NA]
}

ggplot(plot_data, aes(y = State,
                      x = variable)) +
  geom_tile(aes(fill = value_frac)) + 
  geom_text(aes(label = round(value, 2)), size = 2) +
  ggtitle("US States Ranked by Average\nLearning-Adjusted Years of Schooling\nAlongside Other Metrics of\nSchooling and Equity") +
  scale_x_discrete(position = "bottom", expand = c(0,0)) + 
  ylab("US State/Territory, Ranked by Average LAYS 2009-2016") +
  scale_fill_distiller(palette = 'YlGn', direction = 1, na.value = "gray80") +
  coord_fixed(ratio = 0.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 7),
        axis.text  = element_text(size = 7),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 9, hjust = 1),
        legend.position = "none") -> plots
print(plots)

## Save figure as pdf
ggsave(plot = plots,
       filename = "/home/j/temp/hyork/heatmap06022020.pdf",
       width = 7, height = 9)


###############################3
## Plot scatters
###############################
library(ggrepel)
lasy_sum %>% melt(., id.vars = c("NAME", "fips", "year", "measure")) %>% dcast(., NAME + fips + measure + variable ~ year) -> lasy_change
lasy_change[, abs_change := `2016`-`2009`]
states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
lasy_change <- merge(lasy_change, states_shp[c("NAME", "STUSPS")], by = "NAME")



gg1 <- ggplot(lasy_change[measure == "lasy" & variable == "value_all"]) + 
  geom_point(aes(x = `2009`, y = abs_change, color = as.factor(NAME))) +
  geom_text_repel(aes(x = `2009`, y = abs_change, color = as.factor(NAME), label = STUSPS)) + theme_bw() + 
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dotted")+
  xlab("Value in 2009") + ylab("Absolute Change (LAYS)")
gg2 <- ggplot(lasy_change[measure == "yos" & variable == "value_all"]) + 
  geom_point(aes(x = `2009`, y = abs_change, color = as.factor(NAME))) + theme_bw() + 
  geom_text_repel(aes(x = `2009`, y = abs_change, color = as.factor(NAME), label = STUSPS)) +
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dotted")+
  xlab("Value in 2009") + ylab("Absolute Change (Years of Schooling)")
gg3 <- ggplot(lasy_change[measure == "mean_achievement" & variable == "value_all"]) + 
  geom_point(aes(x = `2009`, y = abs_change, color = as.factor(NAME))) + theme_bw() + 
  geom_text_repel(aes(x = `2009`, y = abs_change, color = as.factor(NAME), label = STUSPS)) +
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dotted")+
  xlab("Value in 2009") + ylab("Absolute Change (Mean Achievement)")

pdf("/home/j/temp/hyork/scatters_abs_change_06062020b.pdf", width = 15, height = 5)
grid.arrange(gg1,gg2,gg3, nrow = 1, top = "Absolute Change in LAYS, Compared to Value in 2009, by State")
dev.off()


lasy_change <- merge(lasy_change, pops_race_wide, by = "fips")
lasy_change[, variable := as.character(variable)]
lasy_change[, var := variable]
for(c.race in c("black", "hispanic", "asian", "native")){
  print(c.race)
  lasy_change[var  == paste0("value_", c.race) & get(paste0("mask_", c.race)) == 1,abs_change := NA]
}



lasy_change_race <- dcast(lasy_change, NAME + measure +fips  + STUSPS ~ variable, value.var = "abs_change")

gg1 <- ggplot(lasy_change_race[measure == "lasy"]) +
  geom_point(aes(x = value_white, y = value_hispanic, color = as.factor(NAME))) + theme_bw() + 
  geom_text_repel(aes(x = value_white, y = value_hispanic, color = as.factor(NAME), label = STUSPS)) +
  theme(legend.position = "none") + 
  geom_vline(xintercept = 0, linetype = "dotted")+
  geom_hline(yintercept = 0, linetype = "dotted")+
  lims(x = c(-.25, 1.5), y = c(-.25, 1.5)) +
  coord_equal() +
  xlab("Absolute Change (LAYS), White") + ylab("Absolute Change (LAYS), Hispanic")

gg2 <- ggplot(lasy_change_race[measure == "lasy"]) +
  geom_point(aes(x = value_white, y = value_black, color = as.factor(NAME))) + theme_bw() + 
  geom_text_repel(aes(x = value_white, y = value_black, color = as.factor(NAME), label = STUSPS)) +
  theme(legend.position = "none") + 
  geom_vline(xintercept = 0, linetype = "dotted")+
  lims(x = c(-.25, 1.5), y = c(-.25, 1.5)) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  coord_equal() +  xlab("Absolute Change (LAYS), White") + ylab("Absolute Change (LAYS), Black")
pdf("/home/j/temp/hyork/scatters_abs_change_race_06062020b.pdf", width = 10, height = 5)
grid.arrange(gg1,gg2, nrow = 1, top = "Absolute Change in LAYS, Comparing Black and Hispanic Students\nto White Students, 2009-2016")
dev.off()


###############################
# plot retention standardization
################################

avg_diffs <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/avg_diffs_hs.csv")
avg_diffs[, race := as.character(race)]
#avg_diffs <- avg_diffs[ race != "native"]


gg1 <- avg_diffs[avg_pop > 25 & grade <= 12 & unadj_avg_perc_diff < 5, weighted.mean(avg_perc_diff,avg_pop, na.rm = T), by = .(`Race/Ethnicity`, grade)] %>% ggplot() + 
  geom_line(aes(x = grade-1, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`))+
  geom_point(aes(x = grade-1, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`)) + 
  scale_x_continuous(breaks = seq(1,12,1)) + theme_bw() + xlab("Grade") + ylab(expression(paste(" "[x]," ", italic(p)[Grade]))) + ggtitle("Adjusted") +
  ylim(.8,1.2)+ geom_hline(yintercept = 1, linetype = 2)

gg2 <- avg_diffs[avg_pop > 25 & grade <= 12 & unadj_avg_perc_diff < 5, weighted.mean(unadj_avg_perc_diff,avg_pop, na.rm = T), by = .(`Race/Ethnicity`, grade)] %>% ggplot() + 
  geom_line(aes(x = grade-1, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`))+
  geom_point(aes(x = grade-1, y = V1, color = `Race/Ethnicity`, group = `Race/Ethnicity`)) + 
  scale_x_continuous(breaks = seq(1,12,1))+ theme_bw()+ xlab("Grade") + ylab(expression(paste(" "[x]," ", italic(p)[Grade]))) + ggtitle("Unadjusted") + 
  geom_hline(yintercept = 1, linetype = 2)

pdf("/home/j/temp/hyork/retention_adjustment.pdf", height = 8, width = 10)
grid.arrange(gg1, gg2, nrow = 2)
dev.off()


###############################
# plot OOSPV performance
################################

"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/outputs/oospv_summary.csv" %>% 
  fread() -> oospv

oospv <- oospv[subgroup %in% c("all", "black", "white", "hispanic", 'asian')]
oospv[, subgroup := capitalize(subgroup)]
setnames(oospv, c("Race/Ethnicity", "RMSE", "MAE", "ME"))
stargazer(oospv, summary = F,rownames = F, type = "html", out = "/home/j/temp/hyork/oospv_table.html")

###############################
# plot metrics against constituent parts
################################


lasy[mask == 0 & subgroup != "native" & imputed_step_2 == 0 & imputed_step_1 == 0 & retent_missingness == 0] %>% 
  sample_n(., 10000) -> graph_sample

gg1 <- ggplot(graph_sample) + 
  geom_point(aes(x = yos, y = lasy), alpha = .1) + 
  facet_wrap(~subgroup, ncol = 1) + 
  theme_bw() + ggtitle("Years of Schooling v. Learning-Adjusted YOS")

gg2 <-ggplot(graph_sample) + 
  geom_point(aes(x = yos,y = mean_achievement), alpha = .1) +
  facet_wrap(~subgroup, ncol = 1)+ 
  theme_bw() + ggtitle("YOS v. Learning (Grades +/- Average)")

gg3 <-ggplot(graph_sample) + 
  geom_point(aes(x = yos, y = lasy_capped), alpha = .1)+
  facet_wrap(~subgroup, ncol = 1)+ 
  theme_bw() + ggtitle("Years of Schooling v. LASY (capped at 13)")

pdf("/home/j/temp/hyork/YOS_LASY_scatters.pdf", height = 15, width = 15)
grid.arrange(gg1, gg2, gg3, nrow = 1, top = "n = 10,000 Random Subsample, No Imputation")              
dev.off()


#now scatter races 
lasy[mask == 0 & subgroup != "native" & imputed_step_2 == 0 & imputed_step_1 == 0 & retent_missingness == 0] %>% 
  .[,.(subgroup, leaidC, year, mean_achievement, lasy, yos, lasy_capped, leanm)] %>% 
  melt(., id.vars = c("subgroup", "leaidC", "year", "leanm")) %>% 
  dcast(., leaidC + year + variable  ~ subgroup, fun.aggregate = function(x){mean(x, na.rm = T)}) ->lasy_wide

gg1 <- ggplot(lasy_wide) + 
  geom_point(aes(x = white, y = black), alpha = .1) + 
  facet_wrap(~variable, scales = "free", ncol = 1) + 
  ggtitle("White/Black Scatters of Metrics")

gg2 <- ggplot(lasy_wide) + 
  geom_point(aes(x = white, y = hispanic), alpha = .1) + 
  facet_wrap(~variable, scales = "free", ncol = 1) + 
  ggtitle("White/Hispanic Scatters of Metrics")

gg3 <- ggplot(lasy_wide) + 
  geom_point(aes(x = white, y = asian), alpha = .1) + 
  facet_wrap(~variable, scales = "free", ncol = 1) + 
  ggtitle("White/Asian Scatters of Metrics")

gg4 <- ggplot(lasy_wide) + 
  geom_point(aes(x = black, y = hispanic), alpha = .1) + 
  facet_wrap(~variable, scales = "free", ncol = 1) + 
  ggtitle("Black/Hispanic Scatters of Metrics")

pdf("/home/j/temp/hyork/YOS_LASY_scatters_race.pdf", height = 15, width = 15)
grid.arrange(gg1,gg2,gg3,gg4, nrow = 1)
dev.off()


##############################################
## Variance decomp
##############################################
var_set <- lasy[mask == 0]


##############################################
## Regress on predictors
##############################################
var_set <- lasy[mask == 0]



##############################################
## aggregate as much as possible
##############################################

lasy_agg <- lasy[,.(mean_achievement = mean(mean_achievement, na.rm = T),
                    yos = mean(yos, na.rm = T),
                    lasy = mean(lasy, na.rm = T)
), by = .(subgroup, leaidC,stateabb, mask)]

lasy_agg <- lasy_agg[mask == 0]

lasy_agg_sample <- data.table(sample_n(lasy_agg, 10000))

ggplot(lasy_agg_sample[!subgroup %in% c( "all")]) + 
  geom_point(aes(x = mean_achievement, y = yos, color = subgroup), alpha = .3)


lasy_agg_melt <- melt(lasy_agg, id.vars = c("subgroup", "leaidC", "mask", "stateabb"))
lasy_agg_wide <- dcast(lasy_agg_melt, leaidC  + mask + variable ~ subgroup, value.var = "value")
lasy_agg_wide[, black_white_gap := black - white]
lasy_agg_wide[, hispanic_white_gap := hispanic - white]
lasy_agg_wide[, black_hispanic_gap := black - hispanic]
lasy_agg_wide[, white_asian_gap := white - asian]
lasy_agg_melt <- melt(lasy_agg_wide, c("leaidC", "mask", "variable"), measure.vars = patterns("gap$"))
lasy_agg_wide <- dcast(lasy_agg_melt, leaidC + mask + variable.1 ~ variable)

ggplot(lasy_agg_wide) + 
  geom_point(aes(x = mean_achievement, y = yos), alpha = .1) + 
  facet_wrap(~variable.1) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")

lasy_agg_wide_shp <- merge(lasy_agg_wide, shp, by = "leaidC")

ggplot(lasy_agg_wide_shp[variable.1 == "black_white_gap"]) + 
  geom_sf(aes(fill = mean_achievement, geometry = geometry))


#analyze by state
lasy_agg_shp <- merge(lasy_agg, shp, by = "leaidC")


"/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% 
  fread() -> agg_pops
agg_pops_long <- melt(agg_pops, id.vars = c("GEOID", "NAME"), measure.vars = c("all", "asian", "black", "hispanic", "native", "white"))

lasy_agg_shp <- merge(lasy_agg_shp, agg_pops_long, by.x = c("leaidC", "subgroup"), by.y = c("GEOID", "variable"))

lasy_state_agg <- lasy_agg_shp[,.(mean_achievement = weighted.mean(mean_achievement, value),
                                  yos = weighted.mean(yos, value),
                                  lasy = weighted.mean(lasy, value),
                                  mean_achievement_sd = sqrt(wtd.var(mean_achievement, value)),
                                  yos_sd = sqrt(wtd.var(yos, value)),
                                  lasy_sd = sqrt(wtd.var(lasy, value))), by = .(STATEFP, subgroup)]


states_shp <- read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp")
states_shp <- st_transform(states_shp, st_crs(shp)$proj4string)

lasy_state_agg <- merge(lasy_state_agg, states_shp, by = "STATEFP", all.x = T)

#graph
ggplot(lasy_state_agg[subgroup == "all"]) + 
  geom_bar(aes(x = reorder(NAME, -lasy),y = lasy), stat = "identity") +
  coord_flip()

#
all_subset <- lasy_state_agg[subgroup == "all"]
all_subset <- all_subset[order(yos)]
all_subset[, yos_sort := 1:nrow(all_subset)]
all_subset <- all_subset[order(mean_achievement)]
all_subset[, mean_achievement_sort := 1:nrow(all_subset)]

ggplot(all_subset) + 
  #eom_point(aes(x = yos_sort, y = mean_achievement_sort)) + 
  geom_label(aes(x = yos_sort, y = mean_achievement_sort, label = NAME))

library(ggrepel)
ggplot(all_subset) + 
  geom_point(aes(x = yos, y = mean_achievement)) + 
  geom_label_repel(aes(x = yos, y = mean_achievement, label = NAME))
ggplot(all_subset) + 
  geom_point(aes(x = yos, y = lasy)) + 
  geom_label_repel(aes(x = yos, y = lasy, label = NAME))



dt <- data.table(quant = sample(1:100,100, replace = F))
dt[, index_col := order(quant)]




##############################################
###### make dotplots
##############################################
lasy_agg <- lasy[,.(mean_achievement = mean(mean_achievement, na.rm = T),
                    yos = mean(yos, na.rm = T),
                    lasy = mean(lasy, na.rm = T)
), by = .(subgroup, leaidC,stateabb, mask)]

pops_long[, variable := as.character(variable)]
lasy_agg_pops <- merge(lasy_agg, pops_long, by.x = c("leaidC", "subgroup"), by.y = c("GEOID", "variable"))
lasy_agg_pops[, state_abb := factor(stateabb, levels = 
                                      lasy_agg_pops[subgroup == "all",.( mean = weighted.mean(lasy, population, na.rm = T)), by = stateabb] %>% .[order(mean), stateabb])]

lasy_agg_agg <- lasy_agg_pops[,.(lasy = weighted.mean(lasy, population, na.rm = T),
                                 majority_race = subgroup[population == max(population)]), by = .(leaidC, state_abb)]

lasy_agg_race <- lasy_agg_pops[,.(lasy = weighted.mean(lasy, population, na.rm = T),
                                  yos = weighted.mean(yos, population, na.rm = T),
                                  mean_achievmenet = weighted.mean(mean_achievement, population, na.rm = T),
                                                        population = sum(population)), by = .(subgroup, state_abb)]
lasy_agg_race[, percent_pop := population/sum(population[subgroup != "all"]), by = state_abb]

lasy_agg_race[, mean_lasy := weighted.mean(lasy[subgroup == "all"], population[subgroup == "all"]), by = state_abb]
lasy_agg_race[, mean_yos := weighted.mean(yos[subgroup == "all"], population[subgroup == "all"]), by = state_abb]
lasy_agg_race[, mean_ach := weighted.mean(mean_achievmenet[subgroup == "all"], population[subgroup == "all"]), by = state_abb]

#plot data from line 899 or so
lasy_sum <- lasy_wide[,.(value_all =weighted.mean(value_all, all,na.rm = T),
                         value_black =weighted.mean(value_black, black,na.rm = T),
                         value_hispanic =weighted.mean(value_hispanic, hispanic,na.rm = T),
                         value_white =weighted.mean(value_white, white,na.rm = T),
                         value_asian =weighted.mean(value_asian, asian,na.rm = T),
                         value_native =weighted.mean(value_native, asian,na.rm = T),
                         pop_white = sum(white),
                         pop_asian = sum(asian),
                         pop_black = sum(black),
                         pop_hispanic = sum(hispanic),
                         pop_all = sum(all),
                         pop_native = sum(native)), by = .(year, fips, NAME, variable)]

lasy_sum %>% melt(.,id.vars = c("year", "fips", "NAME", "variable")) %>%
  .[,metric := tstrsplit(variable.1, "_", keep = 1)] %>% 
  .[,subgroup := tstrsplit(variable.1, "_", keep = 2)]  %>% 
dcast(., year + fips + NAME  + variable + subgroup ~ metric, value.var = "value") %>% 
  dcast(., year + fips + NAME + subgroup + pop ~ variable)-> plot_agg_dat

states_shp <- data.table(read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp"))
state_abbs <- unique(states_shp[,.(NAME, STUSPS)])
plot_agg_dat <- merge(plot_agg_dat, state_abbs,by = "NAME")

setnames(plot_agg_dat, "STUSPS", "stateabb")
setnames(plot_agg_dat, "pop", "population")

plot_agg_dat[, state_abb := factor(stateabb, levels = 
                                     plot_agg_dat[subgroup == "all",.( mean = weighted.mean(lasy, population, na.rm = T)), by = stateabb] %>% .[order(mean), stateabb])]


lasy_agg_race <- plot_agg_dat[,.(lasy = weighted.mean(lasy, population, na.rm = T),
                                  yos = weighted.mean(yos, population, na.rm = T),
                                  mean_achievement = weighted.mean(mean_achievement, population, na.rm = T),
                                  population = sum(population)), by = .(subgroup, state_abb, fips)]
lasy_agg_race[, percent_pop := population/population[subgroup == "all"], by = state_abb]

lasy_agg_race[, mean_lasy := weighted.mean(lasy[subgroup == "all"], population[subgroup == "all"]), by = state_abb]
lasy_agg_race[, mean_yos := weighted.mean(yos[subgroup == "all"], population[subgroup == "all"]), by = state_abb]
lasy_agg_race[, mean_ach := weighted.mean(mean_achievement[subgroup == "all"], population[subgroup == "all"]), by = state_abb]


#state sum pops
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% fread()

pops <- census_covs_wide[measure == "population"]

pops_melt <- melt(pops, id.vars = "GEOID", measure.vars = c("all", "asian", "black", "hispanic", "native"))
pops_melt[, fips := as.numeric(str_sub(GEOID, 1, -6))]
pops_race <- pops_melt[,.(sum_st = sum(value)), by = .(fips, variable)]
pops_race_wide <- dcast(pops_race, fips ~ variable)
for(c.race in c("asian", "black", "hispanic", "native")){
  pops_race_wide[, paste0("prop_", c.race) := get(c.race)/all]
  pops_race_wide[get(c.race) < 1000, paste0("mask_", c.race) := 1]
}
na.replace(pops_race_wide, 0) -> pops_race_wide

#merge on data
lasy_agg_race <- merge(lasy_agg_race, pops_race_wide, by = "fips")
for(c.race in c("black", "hispanic", "native", "hispanic", "asian")){
  lasy_agg_race[tolower(subgroup) %like% c.race & get(paste0("mask_", c.race)) == 1,yos := NA]
  lasy_agg_race[tolower(subgroup) %like% c.race & get(paste0("mask_", c.race)) == 1,mean_achievement := NA]
  lasy_agg_race[tolower(subgroup) %like% c.race & get(paste0("mask_", c.race)) == 1,lasy := NA]
  
}

lasy_agg_race[, subgroup := Hmisc::capitalize(subgroup)]


gg1 <- ggplot(lasy_agg_race[population > 1000 & subgroup != "All" & state_abb != "" & !is.na(lasy)]) + 
  geom_line(aes(x = state_abb, y = lasy, group = state_abb), color = "gray20", size = .25)+
  geom_point(aes(x = state_abb, y = lasy, color = subgroup, group = subgroup, size = percent_pop, shape = "By Race/Ethnicity")) + 
  geom_point(aes(x = state_abb, y = mean_lasy, shape = "All Races/Ethnicities" ), color = "black") +
  scale_radius(limits = c(0,1), breaks = seq(0, 1, .2), range =c(1,5), labels = paste0(seq(0,100,20), "%"))+
  scale_color_brewer(palette = "Set1", direction = -1) +
  #scale_color_viridis_d(option = "E") +
  scale_shape_manual(values = c("By Race/Ethnicity" = 19,  "All Races/Ethnicities" = 18)) +
  coord_flip() + 
  theme_bw() + 
  labs(x = "State or Territory", y = "LAYS", title = "Average Learning-Adjusted Years of Schooling (LAYS)",
       color = "Race/Ethnicity", size = "Percent Share of Population", shape = "Aggregation Level") +
  guides(shape = guide_legend(override.aes = list(size=4)))
  
pdf("/home/j/temp/hyork/race_dotplot06112020.pdf", height = 10, width = 6)
print(gg1)
dev.off()

gglegend <- function(x){
  tmp <- ggplot_gtable(ggplot_build(x))
  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
  tmp$grobs[[leg]]
}

legend <- gglegend(gg1)

gg1 <- ggplot(lasy_agg_race[population > 1000 & subgroup != "All" & state_abb != "" & !is.na(lasy)]) + 
  geom_line(aes(x = state_abb, y = lasy, group = state_abb), color = "gray20", size = .25)+
  geom_point(aes(x = state_abb, y = lasy, color = subgroup, group = subgroup, size = percent_pop, shape = "By Race/Ethnicity")) + 
  geom_point(aes(x = state_abb, y = mean_lasy, shape = "All Races/Ethnicities" ), color = "black", size = 2.6) +
  scale_radius(limits = c(0,1), breaks = seq(0, 1, .2), range =c(1,5), labels = paste0(seq(0,100,20), "%"))+
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_shape_manual(values = c("By Race/Ethnicity" = 19,  "All Races/Ethnicities" = 18)) +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(x = "State or Territory", y = "LAYS", title = "Average Learning-Adjusted Years of Schooling (LAYS)",
       color = "Race/Ethnicity", size = "Percent Share of Population", shape = "Aggregation Level")


gg2 <- ggplot(lasy_agg_race[population > 1000 & subgroup != "All" & state_abb != "" & !is.na(mean_achievement)]) + 
  geom_line(aes(x = state_abb, y = mean_achievement, group = state_abb), color = "gray20", size = .25)+
  geom_point(aes(x = state_abb, y = mean_achievement, color = subgroup, group = subgroup, size = percent_pop, shape = "By Race/Ethnicity")) + 
  geom_point(aes(x = state_abb, y = mean_ach, shape = "All Races/Ethnicities" ), color = "black", size = 2.6) +
  scale_radius(limits = c(0,1), breaks = seq(0, 1, .2), range =c(1,5), labels = paste0(seq(0,100,20), "%"))+
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_shape_manual(values = c("By Race/Ethnicity" = 19,  "All Races/Ethnicities" = 18)) +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(x = "State or Territory", y = "Mean Achievement (Grades Ahead/Behind Expected)", title = "Average Mean Achievement in Learning",
       color = "Race/Ethnicity", size = "Percent Share of Population", shape = "Aggregation Level")

gg3 <- ggplot(lasy_agg_race[population > 1000 & subgroup != "All" & state_abb != "" & !is.na(yos)]) + 
  geom_line(aes(x = state_abb, y = yos, group = state_abb), color = "gray20", size = .25)+
  geom_point(aes(x = state_abb, y = yos, color = subgroup, group = subgroup, size = percent_pop, shape = "By Race/Ethnicity")) + 
  geom_point(aes(x = state_abb, y = mean_yos, shape = "All Races/Ethnicities" ), color = "black", size = 2.6) +
  scale_radius(limits = c(0,1), breaks = seq(0, 1, .2), range =c(1,5), labels = paste0(seq(0,100,20), "%"))+
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_shape_manual(values = c("By Race/Ethnicity" = 19,  "All Races/Ethnicities" = 18)) +
  scale_y_continuous(limits = c(11.5, 13), labels = as.character(c(11.5,12, 12.5, 13)), breaks = seq(11.5, 13,.5))+
  
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(x = "State or Territory", y = "YOS", title = "Average Years of Schooling (YOS)",
       color = "Race/Ethnicity", size = "Percent Share of Population", shape = "Aggregation Level")

pdf("/home/j/temp/hyork/race_dotplot06112020_all_metrics.pdf", height = 10, width = 18)
grid.arrange(gg2,gg3,gg1, legend, layout_matrix = cbind(rep(1,5),rep(1,5), rep(2,5),rep(2,5), rep(3,5),rep(3,5), rep(4,5)))
dev.off()
