
## Load libraries
library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)

times =data.frame(num1 = seq(from=0, to=9, by=1),
                  num2 = seq(from=0, to=4, by=1))%>%
  mutate(ann_late_st = 300+365*num1,
         ann_late_en = 330+365*num1,
         bi_late_st =  300+730*num2,
         bi_late_en = 300+730*num2,
         ann_early_st = 120+365*num1,
         ann_early_en = 150+365*num1,
         bi_early_st = 120+730*num2,
         bi_early_en = 150+730*num2)


vax_times=list()
vax_times[["ann_late"]] <- unique(unlist(times$ann_late_st))
vax_times[["bi_late"]] <- unique(unlist(times$bi_late_st))
vax_times[["ann_early"]]<-unique(unlist(times$ann_early_st))
vax_times[["bi_early"]]<-unique(unlist(times$bi_early_st))


vax_times[["ann_435"]] <- 435+365*(c(0,seq(1:8)))
vax_times[["ann_800"]] <- 800+365*(c(0,seq(1:7)))
vax_times[["bi_435"]] <-  435+730*(c(0,seq(1:4)))
vax_times[["bi_800"]] <-  800+730*(c(0,seq(1:3)))

vaxscen <- data.frame(vax_int = c(365,365,365,730,730,730),
                      vax_first=c(0,  435,800,0  ,435,800),
                      scen = c("ann_late","ann_435","ann_800","bi_late","bi_435","bi_800"))

##Get annual number of doses of vaccinations administered and deaths per year    
##For serovax results
nnt_yr <- list()
for(i in 1:n){
   parms <- mod_scen[[i]]$params   ##Get the params from the model run
   
   ##Get the vaccination scenario for interval vaccination based on start time and interval
   vax_int   <- parms[["vax_int"]]
   vax_first <- parms[["vax_first"]]
   index <- which(vaxscen$vax_int==vax_int&vaxscen$vax_first==vax_first)
   scen <- vaxscen$scen[index]
   
   ## get the vax time scenario from scenario runs list
   vax_times_vec <- vax_times[[which(names(vax_times)== scen)]]
   delta3_er <- 0.02
   
   vax <- mod_scen[[i]]$vax_elig %>%
              mutate(yr = floor(time/365))
  
   vax_dose <- vax %>%
     select(time:vax_elig_a, yr)%>%
     filter(time %in% unlist(vax_times_vec))%>%
     mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30)) %>%
     group_by(yr)%>%
     dplyr::summarise(num_dose = sum(est_vax_doses),
                      num_campaigns = n())  
   
   deaths99 <- mod_scen[[i]]$pop_num%>%
     mutate(yr = floor(time/365))
   
   deaths99<- deaths99 %>%
     select(new_Deaths_tot, new_Deaths_c, new_Deaths_a, new_Deaths_e, yr)%>%
     group_by(yr)%>%
     dplyr::summarise(new_Deaths_tot = sum(new_Deaths_tot, na.rm=T),
                      new_Deaths_c = sum(new_Deaths_c, na.rm=T),
                      new_Deaths_a = sum(new_Deaths_a, na.rm=T),
                      new_Deaths_e = sum(new_Deaths_e, na.rm=T))
    
    nnt_yr[[i]]<- deaths99 %>% left_join(vax_dose, by = c("yr"="yr")) %>%
      
      mutate(num_dose =     ifelse(is.na(num_dose),     0,num_dose),
             new_Deaths_e = ifelse(is.na(new_Deaths_e), 0,new_Deaths_e),
             new_Deaths_tot=ifelse(is.na(new_Deaths_tot),0,new_Deaths_tot))%>%
      mutate(num_dose_roll = cumsum(num_dose),
             new_Deaths_e_roll = cumsum(new_Deaths_e),
             new_Deaths_tot_roll = cumsum(new_Deaths_tot),
             sweep_unique = parms[["sweep_unique"]],
             sero_thresh =as.character(scen),
             omega2     =1/parms[["omega2_pc"]],
             omega4     =1/parms[["omega4_pc"]],
             kappa      =1/parms[["kappa3"]],
             foi_sp     =  parms[["foi_sp"]])
}

nnt_yr <- do.call(rbind,nnt_yr)
