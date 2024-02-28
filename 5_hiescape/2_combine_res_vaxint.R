######################################
####Combine res vaxint for FOISWEEP###
######################################

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
hpc_folder <- "/projects/blopman/vger/cliu/00_hiescape"
setwd(hpc_folder)

#setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/DIssertation/Aim 3/COVIDVaxSero_sens/8_varyfoi")
#Read in lists of model results
#####Read in mod_scen for serologically triggered scenarios and no vax scenario


#setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/DIssertation/Aim 3/COVIDVaxSero_sens/8_varyfoi")
##Read in lists of model results
mod_scen <- readRDS("mod_scen_intvax.RDS")

##Combine the pieces into one
#mod_scen <- c(mod_scen1,mod_scen2,mod_scen3,mod_scen4,mod_scen5,mod_scen6)

##Working directories for scripts in Carol's home drive
setwd("/home/cliu369/COVIDVaxSero_sens/8_varyfoi")
##Read in initial sweep scenarios
#sweep <- readRDS("1_main/0_sweep_imm4_foisp0.6_int.RDS")
#sweep2 <- readRDS("1_main/1_sweep_imm4_varyfoi_vax_int_pt2.RDS")
#sweep3 <- readRDS("1_main/1_sweep_imm4_varyfoi_vax_int_pt3.RDS")
#sweep_unique <- readRDS("0_postprocess/00_sweep_unique.RDS")

#sweep <- rbind(sweep1,sweep2,sweep3)


#Number of model runs needing processing
n   <- length(mod_scen)

##Inputs: n, mod_scen, sweep associated with mod_scen

#####################################################
##OUTPUTS:vax dose and deaths needed for nnt#########
#####################################################

source("0_postprocess/0_nnt_int.R")


######################################################
##OUTPUTS: Time series case, sero, and death###########
######################################################

source("0_postprocess/0_case_sero_death_timeseries_int.R")


##############################################################
##OUTPUTS: Time series immunity tiers and median value#########
##############################################################
#list1 <- which(sweep$sero_thresh %in% c(0, 0.5,0.6,0.7,0.8))
list1  <- seq(1:n)
source("0_postprocess/0_imm_timeseries_int.R")


###########################################################
##OUTPUT: Year NNT#########################################
##########################################################
#source("0_postprocess/1_annual_int.R")

setwd(hpc_folder)
saveRDS(nnt,    "0_comb_res/vaxint_nnt.RDS")
saveRDS(inc,    "0_comb_res/vaxint_inc.RDS")
saveRDS(sero,   "0_comb_res/vaxint_sero.RDS")
saveRDS(imm,    "0_comb_res/vaxint_imm.RDS")
saveRDS(imm_med,"0_comb_res/vaxint_imm_med.RDS")
saveRDS(deaths, "0_comb_res/vaxint_death.RDS")
#saveRDS(nnt_yr, "0_comb_res/vaxint_nntyr.RDS")


