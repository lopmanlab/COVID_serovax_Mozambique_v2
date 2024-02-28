
## Load libraries
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)


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
    
    mutate(num_dose =     ifelse(is.na(num_dose),     0,num_dose),
           new_Deaths_e = ifelse(is.na(new_Deaths_e), 0,new_Deaths_e),
           new_Deaths_tot=ifelse(is.na(new_Deaths_tot),0,new_Deaths_tot))%>%
    mutate(num_dose_roll = cumsum(num_dose),
           new_Deaths_e_roll = cumsum(new_Deaths_e),
           new_Deaths_tot_roll = cumsum(new_Deaths_tot),
           #sweep_unique = sweep$sweep_unique[i],
           r02         = parms[["r02"]],
           r03         = parms[["r03"]],
           sero_thresh = parms[["sero_thresh"]],
           #sero_thresh =as.character(sweep$sero_thresh[i]),
           omega2     =1/parms[["omega2_pc"]],
           omega4     =1/parms[["omega4_pc"]],
           kappa      =1/parms[["kappa3"]],
           foi_sp     =  parms[["foi_sp"]])
}

nnt_yr <- do.call(rbind,nnt_yr)
