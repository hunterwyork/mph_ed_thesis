library(data.table)
library(ggplot2)
library(reldist, lib.loc = '/home/j/temp/hyork/rlibs')
library(magrittr)

lasy <- fread("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/lasy_aggregations/all.csv")
lasy_wide <- dcast(lasy[subgroup != "native"], leaidC + year  + fips ~ subgroup, value.var = "lasy")

#census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/census_covs_wide.csv" %>% fread()
census_covs_wide <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/ref/ccd_pops_for_agg.csv" %>% fread()
pops <- census_covs_wide[measure == "population"]
for(c.race in c("asian", "black", "hispanic", "white")){
  pops[get(c.race) < 100, (paste0("mask_", c.race)) := 1]
  pops[is.na(get(paste0("mask_", c.race))), (paste0("mask_", c.race)) := 0]
}
pops[, mask_all := 0]
pops %>% na.replace(., 0) -> pops
pops_melt <- melt(pops[,.(GEOID, mask_asian, mask_black, mask_white, mask_hispanic, mask_all)], id.var = "GEOID")
pops_melt[, subgroup := tstrsplit(variable, "_",keep = 2)]
setnames(pops, c("all", "white","black", "asian", "hispanic"), c('total_pop', paste0("total_", c("white","black", "asian", "hispanic"))))
#pops <- pops[ccd == 0]
lasy_wide_pop <- merge(lasy_wide, pops[,.(GEOID, NAME, prop_all, prop_asian, prop_black, prop_hispanic,prop_white, total_pop, total_asian, total_black, total_hispanic, total_white)], by.x = "leaidC", by.y = "GEOID")

lasy_wide_pop[, gini_bw_sd := gini(all, total_pop), by = .(fips, year)]
lasy_wide_pop[, gini_wi_sd := gini(c(asian, black, hispanic, white), c(prop_asian, prop_black, prop_hispanic, prop_white)), by = .(leaidC, year)]
lasy_wide_pop[, gini_tot_state := gini(c(asian, black, hispanic, white), c(total_asian, total_black, total_hispanic, total_white)), by = .(fips, year)]
lasy_wide_pop[, sum_lasy_state := sum((all*total_pop)), by = .(fips, year)]
lasy_wide_pop[, sum_pop_state := sum(total_pop), by = .(fips, year)]
lasy_wide_pop[, perc_lasy_dist :=( all*total_pop)/sum_lasy_state]
lasy_wide_pop[, perc_pop_dist :=total_pop/sum_pop_state]
lasy_wide_pop[, tot_wi_sd_gini_state := sum(perc_lasy_dist * gini_wi_sd, na.rm = T), by = .(year, fips)]
lasy_wide_summary <- unique(lasy_wide_pop[,.(year, fips, gini_bw_sd, gini_tot_state, sum_pop_state, tot_wi_sd_gini_state)])

ggplot(lasy_wide_summary, aes(x = year , y = gini_tot_state, group = as.factor(fips), color = as.factor(fips))) + geom_line()

states_shp <- data.table(read_sf("/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/reference/cb_2018_us_state_20m.shp"))
states_shp[, fips := as.numeric(STATEFP)]
lasy_wide_summary <- merge(lasy_wide_summary, states_shp[,.(fips, NAME)], by = "fips")
l
##plot
ggplot(lasy_wide_summary[year == 2016 & !is.na(gini_tot_state)],
       aes(x = reorder(NAME, gini_tot_state), y = gini_tot_state)) + geom_bar(stat = "identity") + coord_flip()
ggplot(lasy_wide_summary[year == 2016 & !is.na(gini_tot_state)],
       aes(x = reorder(NAME, tot_wi_sd_gini_state/gini_tot_state), y = tot_wi_sd_gini_state/gini_tot_state)) + geom_bar(stat = "identity") + coord_flip()
