#######################################
####Combine res vaxsero for FOISWEEP###
#######################################

library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)

##Metadata for populations
tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop

pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)

#####Read in mod_scen for serologically triggered scenarios and no vax scenario
hpc_folder <- "/projects/blopman/vger/cliu/00_sens_varyfoi"
setwd(hpc_folder)

#setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/DIssertation/Aim 3/COVIDVaxSero_sens/8_varyfoi")
##Read in lists of model results
mod_scen1 <- readRDS("mod_scen_vax_foisp0.5.RDS")
mod_scen2 <- readRDS("mod_scen_vax_foisp0.6.RDS")
mod_scen3 <- readRDS("mod_scen_vax_foisp0.7.RDS")
mod_scen4 <- readRDS("mod_scen_vax_foisp0.8.RDS")
mod_scen5 <- readRDS("mod_scen_vax_foisp0.9.RDS")

##Combine the pieces into one
mod_scen <- c(mod_scen1,mod_scen2,mod_scen3,mod_scen4,mod_scen5)



##Working directories for scripts in Carol's home drive
setwd("/home/cliu369/COVIDVaxSero_sens/8_varyfoi")
##Read in initial sweep scenarios
#sweep <- readRDS("1_main/0_sweep_imm4_foisp0.6.RDS")
##Get the sweep unique to correctly identify from the r0 draws
sweep_unique <- readRDS("0_postprocess/00_sweep_unique.RDS")

##Number of model runs needing processing
n   <- length(mod_scen)

##Inputs for all below: n, mod_scen, sweep associated with mod_scen

####################################################
##OUTPUT:vax dose and deaths needed for nnt########
####################################################

source("0_postprocess/9_nnt_sero.R")
nnt <- nnt%>%
       select(-sweep_unique)%>%
      left_join(sweep_unique%>%select(sweep_unique:r03), by=c("r02","r03"))

######################################################
##OUTPUT: Time series case, sero, and death###########
######################################################

source("0_postprocess/9_case_sero_death_timeseries.R")
inc    <- inc%>%   left_join(sweep_unique%>%select(sweep_unique:r03), by =c("r02","r03"))
deaths <- deaths%>%left_join(sweep_unique%>%select(sweep_unique:r03), by =c("r02","r03"))
sero   <- sero%>%  left_join(sweep_unique%>%select(sweep_unique:r03), by =c("r02","r03"))
##########################################################
##OUTPUT: Time series immunity tiers and median value#####
##########################################################

#list1 <- which(sweep$sero_thresh %in% c(0, 0.5,0.6,0.7,0.8))
list1 <- seq(1:n)
source("0_postprocess/9_imm_timeseries.R")
imm   <- imm%>%  left_join(sweep_unique%>%select(sweep_unique:r03), by =c("r02","r03"))

#imm <- imm%>%filter(sero_thresh%in% c("0","0.5","0.65","0.8"))
#imm_med <- imm_med%>%filter(sero_thresh%in% c("0","0.5","0.65","0.8"))

###########################################################
##OUTPUT: Year NNT#########################################
##########################################################
#source("0_postprocess/1_annual_sero.R")

setwd(hpc_folder)
saveRDS(nnt,    "0_comb_res/vaxsero_nnt.RDS")
saveRDS(inc,    "0_comb_res/vaxsero_inc.RDS")
saveRDS(sero,   "0_comb_res/vaxsero_sero.RDS")
saveRDS(imm,    "0_comb_res/vaxsero_imm.RDS")
saveRDS(imm_med,"0_comb_res/vaxsero_imm_med.RDS")
saveRDS(deaths, "0_comb_res/vaxsero_death.RDS")
#saveRDS(nnt_yr, "0_comb_res/vaxsero_nntyr.RDS")

