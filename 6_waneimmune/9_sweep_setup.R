library("dplyr")

sweep_check <- readRDS("../1_main0.7/0_sweep_imm4_foisp0.7.RDS")


sweep<- readRDS("../3_foisweep/0_sweep_imm4_varyfoi_vax.RDS")
sweep_foisp0.7_kappa2500 <- sweep%>%
                            filter(foi_sp==0.7)%>%
                            filter(kappa2==1/2500)

sweep0 <- sweep_foisp0.7_kappa2500%>%
          mutate(sero_thresh=0)%>%unique()

sweep1 <- rbind(sweep0,sweep_foisp0.7_kappa2500)

sweep_sero <- rbind(sweep1,sweep1)
sweep_sero <- sweep_sero%>%
              mutate(omega2_pc = rep(c(1/90,1/365),each=256),
                     omega4_pc = rep(c(1/365,1/730), each=256))

saveRDS(sweep_sero, "0_sweep_sero_waneimm.RDS")

sweep2 <- sweep_sero%>%filter(sero_thresh==0)%>%unique()
sweep_int<- rbind(sweep2,sweep2)

sweep_int<- sweep_int%>%
  mutate(vax_first = 0,
         vax_int   = rep(c(365,730),         each=64),
         vax_start = rep(c(300/365, 300/730),each=64),
         vax_end   = rep(c(330/365, 330/730),each=64))

saveRDS(sweep_int, "0_sweep_int_waneimm.RDS")