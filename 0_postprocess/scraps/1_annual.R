
## Load libraries
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)




#####Read in mod_scen for serologically triggered scenarios and no vax scenario
setwd("/projects/blopman/vger/cliu/00_sens_varyfoi/0_foi0.6_kappa2500")

#setwd("C:/Users/cliu369/OneDrive - Emory University/Documents/DIssertation/Aim 3/COVIDVaxSero_sens/8_varyfoi")
#Read in lists of model results
mod_scen1 <- readRDS("mod_scen_vaxsero1.RDS")
mod_scen2 <- readRDS("mod_scen_vaxsero2.RDS")
mod_scen3 <- readRDS("mod_scen_vaxsero3.RDS")
mod_scen4 <- readRDS("mod_scen_vaxsero4.RDS")
#mod_scen5 <- readRDS("mod_scen_vax_foisp0.9.RDS")
#Combine the pieces into one
mod_scen <- c(mod_scen1,mod_scen2,mod_scen3,mod_scen4)

##Working directories for scripts in Carol's home drive
setwd("/home/cliu369/COVIDVaxSero_sens/8_varyfoi")
##Read in initial sweep scenarios
sweep <- readRDS("1_main/0_sweep_imm4_foisp0.6.RDS")

#Number of model runs needing processing
n   <- length(mod_scen)

##Get annual number of doses of vaccinations administered and deaths per year    
##For serovax results
nnt_yr <- list()
for(i in 1:n){
   parms <- mod_scen[[i]]$params   ##Get the params from the model run
  
   vax <- mod_scen[[i]]$vax_elig %>%
              mutate(yr = floor(time/365))
  
  vax_dose <- vax %>% 
            mutate(flag = ifelse(delta3_er == 0.02 & lag(delta3_er==0),1,0))%>%
            filter(flag==1)%>%
            mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
  vax_dose <- vax_dose %>%
            group_by(yr)%>%
            dplyr::summarise(num_dose = sum(est_vax_doses),
                             num_campaigns = n())
  
  deaths <- mod_scen[[i]]$pop_num%>%
              mutate(yr = floor(time/365))
  
  deaths<- deaths %>%
    select(new_Deaths_tot, new_Deaths_c, new_Deaths_a, new_Deaths_e, yr)%>%
    group_by(yr)%>%
    dplyr::summarise(new_Deaths_tot = sum(new_Deaths_tot, na.rm=T),
                     new_Deaths_c = sum(new_Deaths_c, na.rm=T),
                     new_Deaths_a = sum(new_Deaths_a, na.rm=T),
                     new_Deaths_e = sum(new_Deaths_e, na.rm=T))
  
  nnt_yr[[i]]<- deaths %>% left_join(vax_dose, by = c("yr"="yr")) %>%
    mutate(num_dose_roll = cumsum(num_dose),
           new_Deaths_e_roll = cumsum(new_Deaths_e),
           new_Deaths_tot_roll = cumsum(new_Deaths_tot),
           sweep_unique = sweep$sweep_unique[i],
           sero_thresh = parms[["sero_thresh"]],
           #sero_thresh =as.character(sweep$sero_thresh[i]),
           omega2     =1/parms[["omega2_pc"]],
           omega4     =1/parms[["omega4_pc"]],
           kappa      =1/parms[["kappa3"]],
           foi_sp     =  parms[["foi_sp"]])
}

nnt_yr <- do.call(rbind,nnt_yr)
saveRDS()
