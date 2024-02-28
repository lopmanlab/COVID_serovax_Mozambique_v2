library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(gridExtra)



#sweep_int1 <- readRDS("3_foisweep/1_sweep_imm4_varyfoi_vax_int_pt1.RDS")
#sweep_int2 <- readRDS("3_foisweep/1_sweep_imm4_varyfoi_vax_int_pt2.RDS")
#sweep_int3 <- readRDS("3_foisweep/1_sweep_imm4_varyfoi_vax_int_pt3.RDS")

#sweep_int<- rbind(sweep_int1, sweep_int2, sweep_int3)

#saveRDS(sweep_int, "3_foisweep/1_sweep_imm4_varyfoi_vax_int.RDS")


##Combing the first 32 sweep_uniques with the remaining
##Did these separately to save on computation time
## First do for vaxint
vaxint_death <- readRDS("../3_foisweep/0_res/vaxint_death.RDS")
#vaxint_imm_med <- readRDS("3_foisweep/0_res/vaxint_imm_med.RDS")
vaxint_imm <- readRDS("../3_foisweep/0_res/vaxint_imm.RDS")
vaxint_inc <- readRDS("../3_foisweep/0_res/vaxint_inc.RDS")
vaxint_nnt <- readRDS("../3_foisweep/0_res/vaxint_nnt.RDS")
vaxint_sero<- readRDS("../3_foisweep/0_res/vaxint_sero.RDS")

vaxint_death  <- vaxint_death%>%filter(foi_sp==0.7)
#vaxint_imm_med<- vaxint_imm_med%>%filter(foi_sp==0.7)
vaxint_imm    <- vaxint_imm    %>%filter(foi_sp==0.7)
vaxint_inc    <- vaxint_inc    %>%filter(foi_sp==0.7)
vaxint_nnt    <- vaxint_nnt    %>%filter(foi_sp==0.7)
vaxint_sero   <- vaxint_sero   %>%filter(foi_sp==0.7)

vaxint_death_main  <- readRDS("0_res/orig/vaxint_death.RDS")
#vaxint_imm_med_main<- readRDS("1_main/0_res/orig/vaxint_imm_med.RDS")
vaxint_imm_main    <- readRDS("0_res/orig/vaxint_imm.RDS")
vaxint_inc_main    <- readRDS("0_res/orig/vaxint_inc.RDS")
vaxint_nnt_main    <- readRDS("0_res/orig/vaxint_nnt.RDS")
vaxint_sero_main   <- readRDS("0_res/orig/vaxint_sero.RDS")

vaxint_death_comb  <- bind_rows(vaxint_death_main,  vaxint_death)
#vaxint_imm_med_comb<- rbind(vaxint_imm_med_main,vaxint_imm_med)
vaxint_imm_comb    <- bind_rows(vaxint_imm_main    ,vaxint_imm)
vaxint_inc_comb    <- bind_rows(vaxint_inc_main    ,vaxint_inc)
vaxint_nnt_comb    <- bind_rows(vaxint_nnt_main    ,vaxint_nnt)
vaxint_sero_comb   <- bind_rows(vaxint_sero_main   ,vaxint_sero)
vaxint_imm_med_comb<- vaxint_imm_comb%>%group_by(date,sero_thresh,imm_cat, age_grp, omega,kappa, foi_sp)%>%
                      dplyr::summarise(med = median(val))

##Checks
vaxint_death_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp,sweep_unique)%>%dplyr::summarise(n=n())
vaxint_imm_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp,sweep_unique)%>%dplyr::summarise(n=n())
vaxint_imm_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp)%>%dplyr::summarise(n=n())
vaxint_inc_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp)%>%dplyr::summarise(n=n())
vaxint_inc_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vaxint_sero_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vaxint_imm_med_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vaxint_nnt_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())

saveRDS(vaxint_death_comb, "0_res/vaxint_death.RDS")
saveRDS(vaxint_imm_comb, "0_res/vaxint_imm.RDS")
saveRDS(vaxint_inc_comb, "0_res/vaxint_inc.RDS")
saveRDS(vaxint_nnt_comb, "0_res/vaxint_nnt.RDS")
saveRDS(vaxint_sero_comb,"0_res/vaxint_sero.RDS")
saveRDS(vaxint_imm_med_comb, "0_res/vaxint_imm_med.RDS")

rm(list=ls())

##Now do for vax sero and novax
##Read in vax sero from first 32 sweep_unique
vax_death <- readRDS("3_foisweep/0_res/vaxsero_death.RDS")
vax_imm <- readRDS("3_foisweep/0_res/vaxsero_imm.RDS")
vax_inc <- readRDS("3_foisweep/0_res/vaxsero_inc.RDS")
vax_nnt <- readRDS("3_foisweep/0_res/vaxsero_nnt.RDS")
vax_sero<- readRDS("3_foisweep/0_res/vaxsero_sero.RDS")

vax_death  <- vax_death  %>%filter(foi_sp==0.7)%>%mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)
vax_imm    <- vax_imm    %>%filter(foi_sp==0.7)%>%mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)
vax_inc    <- vax_inc    %>%filter(foi_sp==0.7)%>%mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)
vax_nnt    <- vax_nnt    %>%filter(foi_sp==0.7)%>%mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)
vax_sero   <- vax_sero   %>%filter(foi_sp==0.7)%>%mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)
vax_nnt    <- vax_nnt%>%
              mutate(sero_thresh = as.character(sero_thresh))%>%filter(kappa==2500)

##Read in no vax from first 32 sweep_unique
novax_death<- readRDS("3_foisweep/0_res/novax_death.RDS")%>%filter(foi_sp==0.7)%>%filter(kappa==2500)%>%mutate(sero_thresh = as.character(sero_thresh))
novax_imm  <- readRDS("3_foisweep/0_res/novax_imm.RDS")%>%filter(foi_sp==0.7)%>%filter(kappa==2500)%>%mutate(sero_thresh = as.character(sero_thresh))
novax_inc  <- readRDS("3_foisweep/0_res/novax_inc.RDS")%>%filter(foi_sp==0.7)%>%filter(kappa==2500)%>%mutate(sero_thresh = as.character(sero_thresh))
novax_nnt  <- readRDS("3_foisweep/0_res/novax_nnt.RDS")%>%filter(foi_sp==0.7)%>%filter(kappa==2500)%>%
                            mutate(sero_thresh = as.character(sero_thresh))
novax_sero <- readRDS("3_foisweep/0_res/novax_sero.RDS")%>%filter(foi_sp==0.7)%>%filter(kappa==2500)%>%mutate(sero_thresh = as.character(sero_thresh))



vax_death_main  <- readRDS("1_main0.7/0_res/orig/vaxsero_death.RDS")
vax_imm_main    <- readRDS("1_main0.7/0_res/orig/vaxsero_imm.RDS")
vax_inc_main    <- readRDS("1_main0.7/0_res/orig/vaxsero_inc.RDS")
vax_nnt_main    <- readRDS("1_main0.7/0_res/orig/vaxsero_nnt.RDS")
vax_sero_main   <- readRDS("1_main0.7/0_res/orig/vaxsero_sero.RDS")

##Bind them together
vax_death_comb  <- bind_rows(vax_death_main,vax_death, novax_death)
vax_imm_comb    <- bind_rows(vax_imm_main, vax_imm, novax_imm)
vax_inc_comb    <- bind_rows(vax_inc_main, vax_inc, novax_inc)
vax_sero_comb   <- bind_rows(vax_sero_main, vax_sero, novax_sero)
vax_imm_med_comb<- vax_imm_comb%>%group_by(date,sero_thresh,imm_cat, age_grp, omega,kappa, foi_sp)%>%
                      dplyr::summarise(med = median(val))

vax_nnt_comb    <- bind_rows(vax_nnt,novax_nnt,vax_nnt_main)

##Checks
vax_death_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp,sweep_unique)%>%dplyr::summarise(n=n())
vax_imm_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp,sweep_unique)%>%dplyr::summarise(n=n())
vax_imm_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp)%>%dplyr::summarise(n=n())
vax_inc_comb%>%group_by(sero_thresh, omega2,kappa,foi_sp)%>%dplyr::summarise(n=n())
vax_inc_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vax_sero_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vax_imm_med_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())%>%mutate(n1=n/3650)
vax_nnt_comb%>%group_by(sero_thresh)%>%dplyr::summarise(n=n())

#novax_nnt_main  <- vax_nnt_main%>%filter(sero_thresh=="0")
saveRDS(vax_death_comb, "1_main0.7/0_res/vaxsero_death.RDS")
saveRDS(vax_imm_comb, "1_main0.7/0_res/vaxsero_imm.RDS")
saveRDS(vax_inc_comb, "1_main0.7/0_res/vaxsero_inc.RDS")
saveRDS(vax_nnt_comb, "1_main0.7/0_res/vaxsero_nnt.RDS")
saveRDS(vax_sero_comb,"1_main0.7/0_res/vaxsero_sero.RDS")
saveRDS(vax_imm_med_comb, "1_main0.7/0_res/vaxsero_imm_med.RDS")



