########################################################################
## Hunter York, hunterwyork@gmail.com
#####################################
## This code pulls in out of sample predictive validity estimates and
## compares them to known truths to derive metrics of performance of the model
## 
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
imputed_data <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data/" %>% 
  list.files(., full.names = T) %>% mclapply(., fread, mc.cores = cores) %>% rbindlist(., fill = T)

imputed_data_holdouts <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_holdouts/" %>% 
  list.files(., full.names = T) %>% mclapply(., fread, mc.cores = cores) %>% rbindlist(., fill = T)

imputed_data[, grade := as.numeric(grade)]
imputed_data_holdouts[, mean_achievement_truth := NULL]
#merge on orig heldout data
setnames(imputed_data, "mean_achievement", "mean_achievement_truth")
imputed_data_holdouts[, grade := as.numeric(as.character(grade))]
imputed_data_holdouts[, leidC := as.numeric(as.character(leaidC))]
imputed_data_holdouts[, year := as.numeric(as.character(year))]

merge1 <- merge(imputed_data_holdouts[data_ho ==1], imputed_data[,.(subgroup, leaidC, year, fips, grade, subject, mean_achievement_truth, imputed_step_1, imputed_step_2)], by = c("subgroup", "leaidC", "year", "fips", "grade", "subject"))

#calculate oos stuff
merge1[,error := mean_achievement - mean_achievement_truth]
summary1 <- merge1[error != 0,.(rmse = sqrt(mean(error^2, na.rm = T)),
                                  mae = median(abs(error), na.rm = T),
                                  me = median(error,na.rm = T) ), by = .(subgroup)]


###################
## Now repeat for learning
##################

#read data
imputed_data <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent/" %>% 
  list.files(., full.names = T) %>% mclapply(., fread, mc.cores = cores) %>% rbindlist(., fill = T)

imputed_data_holdouts <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/imputed_data_retent_holdout/" %>% 
  list.files(., full.names = T) %>% mclapply(., fread, mc.cores = cores) %>% rbindlist(., fill = T)

imputed_data[, grade := as.numeric(grade)]
imputed_data_holdouts[, mean_achievement_truth := NULL]
#merge on orig heldout data
setnames(imputed_data, "mean_achievement", "mean_achievement_truth")
imputed_data_holdouts[, grade := as.numeric(as.character(grade))]
imputed_data_holdouts[, leidC := as.numeric(as.character(leaidC))]
imputed_data_holdouts[, year := as.numeric(as.character(year))]

merge2 <- merge(imputed_data_holdouts[data_ho == 1], imputed_data[,.(subgroup, leaidC, year, fips, grade, subject, mean_achievement_truth, imputed_step_1, imputed_step_2)], by = c("subgroup", "leaidC", "year", "fips", "grade", "subject"))

#calculate oos stuff
merge2[,error := mean_achievement - mean_achievement_truth]
summary2 <- merge2[error != 0,.(rmse = sqrt(mean(error^2, na.rm = T)),
                              mae = median(abs(error), na.rm = T),
                              me = median(error,na.rm = T) ), by = .(subgroup)]

summary1[, subject := "Learning"]
summary2[, subject := "Retention"]

summary_all <- rbind(summary1, summary2)
summary_all[, subgroup := Hmisc::capitalize(subgroup)]
setnames(summary_all, c("subgroup", "Root Mean Squared Error", "Median Absolute Error", "Median Error", "subject"))
summary_all <- melt(summary_all, id.vars = c("subgroup", "subject"))
setnames(summary_all, "subgroup", "Race/Ethnicity")

gg1 <- ggplot(summary_all[subject == "Learning"]) + 
  geom_bar(aes(x = `Race/Ethnicity`, y = value, fill = `Race/Ethnicity` ), stat = "identity") + 
  geom_text(aes(x = `Race/Ethnicity`, y = value + .03, label  = round(value, 3)))+
  facet_grid(subject~variable, scales = "free") + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set2") + 
  ylab("Error (as a Grade-Level Equivalents)") + theme(legend.position = "none")
gg2 <- ggplot(summary_all[subject == "Retention"]) + 
  geom_bar(aes(x = `Race/Ethnicity`, y = value, fill = `Race/Ethnicity` ), stat = "identity") + 
  geom_text(aes(x = `Race/Ethnicity`, y = value + .0025, label  = round(value, 3)))+
  facet_grid(subject~variable, scales = "free") + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set2") + 
  ylab("Error (as a Probability)") +   theme(legend.position = "none")

gglegend <- function(x){
  tmp <- ggplot_gtable(ggplot_build(x))
  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
  tmp$grobs[[leg]]
}
legend <- gglegend(ggplot(summary_all[subject == "Retention"]) + 
                     geom_bar(aes(x = `Race/Ethnicity`, y = value, fill = `Race/Ethnicity` ), stat = "identity") + 
                     geom_text(aes(x = `Race/Ethnicity`, y = value + .0025, label  = round(value, 3)))+
                     facet_grid(subject~variable, scales = "free") + 
                     theme_bw() + 
                     scale_fill_brewer(palette = "Set2") + 
                     ylab("Error (as "))

pdf("/home/j/temp/hyork/error_figure.pdf", height = 7, width = 12)
grid.arrange(gg1, gg2, legend, layout_matrix = rbind(c(1,1,1,1,1), c(2,2,2,2,2)), 
             top = c("Summary Statistics of Errors for Three-Fold\nOut of Sample Predictive Validity Tests"))
dev.off()
