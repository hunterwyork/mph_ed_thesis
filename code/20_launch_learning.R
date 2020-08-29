dir <- "/scratch/network/hyork/projects/mph_ed_thesis/"
system(paste0("sbatch --time=00:20:00 --job-name=thesis_load_data", 
" --tasks=1 --cpus-per-task=1 --mem-per-cpu=30G ",
"--chdir ", dir,
" --export=ALL,script_path=", dir, "code/21_seda_inputs.R ",
"/scratch/network/hyork/shells/base_r.sh"))



c.proj <- "proj_team"
queue <- "long.q"
wd <- "/home/j/WORK/01_covariates/02_inputs/education/update_2020/geospatial_final_project/code/"


## step 1
system(paste0("qsub -N edu_geo_1 -P  ",c.proj," -l fthread=3 -l m_mem_free=30G -q ", queue,
              " -l h_rt=0:30:00 -l archive=TRUE ",code_d,"shells/r_shell_35107.sh ",wd,"21_seda_inputs.R"))

## step 2
n_jobs <- 10
code_d <- "/share/covariates/education/code/model/"
system(paste0("qsub -t 1:", n_jobs," -N edu_geo_2 -P  ",c.proj," -l fthread=3 -l m_mem_free=30G -q ", queue,
              " -l h_rt=0:30:00 -l archive=TRUE ",code_d,"shells/r_shell_35107.sh ",wd,"22_plot_seda.R"))

## step 3
n_jobs <- 510
system(paste0("qsub -t 1:", n_jobs," -N edu_geo_3 -P  ",c.proj," -l fthread=1 -l m_mem_free=10G -q ", queue,
              " -l h_rt=3:00:00 -l archive=TRUE ",code_d,"shells/r_shell_35107.sh ",wd,"23_impute_seda.R"))


## step 4
n_jobs <- 510
system(paste0("qsub -N edu_geo_4 -P  ",c.proj," -l fthread=1 -l m_mem_free=10G -q ", queue,
              " -l h_rt=3:00:00 -l archive=TRUE ",code_d,"shells/r_shell_35107.sh ",wd,"24_oos_pv.R"))


## step 5
n_jobs <- 10
system(paste0("qsub -t 1:", n_jobs," -N edu_geo_5 -P  ",c.proj," -l fthread=1 -l m_mem_free=10G -q ", queue,
              " -l h_rt=3:00:00 -l archive=TRUE ",code_d,"shells/r_shell_35107.sh ",wd,"25_aggregate_imputed.R"))

## step 6