library("dplyr")

sw_hi <- readRDS("0_sweep_sero.RDS")
sw_imm4_foi0.7<- readRDS("3_foisweep/0_sweep_imm4_varyfoi_novax.RDS")

sw_imm4_foi0.7<- sw_imm4_foi0.7%>%filter(foi_sp==0.7)%>%filter(kappa3==1/2500)

##Main difference is inc_trans==1.075 for the hi escape strategy, or else the same
sweep <- sw_imm4_foi0.7%>%
          mutate(inc_trans=1.075)

saveRDS(sweep,"0_sweep_hiescape_novax.RDS")

##Add in the vax scenarios
sweep<-readRDS("0_sweep_hiescape_novax.RDS")
sweep1 <- rbind(sweep,sweep,sweep,sweep,sweep,sweep,sweep)
sweep1 <- sweep1%>%
          mutate(sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=32))

saveRDS(sweep1, "0_sweep_hiescape_serovax.RDS")

##Add in sweepint scenario
sweep2 <- rbind(sweep, sweep)
sweep2<- sweep2%>%
  mutate(vax_first = 0,
         vax_int   = rep(c(365,730),         each=32),
         vax_start = rep(c(300/365, 300/730),each=32),
         vax_end   = rep(c(330/365, 330/730),each=32))
saveRDS(sweep2, "0_sweep_hiescape_intvax.RDS")

