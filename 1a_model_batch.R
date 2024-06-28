
##Code below is Rscript that is executed through the bash script calib_bash.sh

library(deSolve)
library(tidyverse)
require(plyr)
library(here)
library(foreach)
library(progressr)
library(doParallel)
library(tictoc)

tic()

#rm(list=ls())
#cl <- makeCluster(8)
#registerDoParallel(cl)

##For the HPC
registerDoParallel(cores=32)
options(scipen=999)

##Edit beginning and end of sweep to execute the sweeps across the four cores
getwd()


sweep_beg <- 1
sweep_end <- 32

mod_scenarios <- foreach(i = sweep_beg:sweep_end) %dopar% {
  #mod_scenarios <- for(i in 1:num_sweep){
  library(deSolve)
  library(tidyverse)
  library(plyr)
  
  sweep<-sweep
  source("0_model/0_model_setup.R")
  
  
  loop<- model_sims(i)
}
toc()

#saveRDS(mod_scenarios,"/projects/blopman/vger/cliu/00_sens_extremescen/mod_scen_sd_vei_kappa3.RDS")
#saveRDS(mod_scenarios,"/projects/lau_projects/gametapop/0_covid_moz_sens/00_sens_extremescen/mod_scen_sd_vei_vax6_1_vax95.RDS")

