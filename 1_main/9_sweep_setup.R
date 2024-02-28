sweep_sero <- readRDS("../1_main/0_sweep_imm4_foisp0.6.RDS")
sweep_sero1 <- sweep_sero%>%filter(sweep_unique<=96)
sweep_sero1 <- sweep_sero1%>%mutate(foi_sp=0.7,foi_sn=1.3)


sweep_int  <- readRDS("../1_main/0_sweep_imm4_foisp0.6_int.RDS")
sweep_int1 <- sweep_int%>%filter(sweep_unique<=96)
sweep_int1 <- sweep_int1%>%mutate(foi_sp=0.7,foi_sn=1.3)



saveRDS(sweep_sero1, "../1_main0.7/0_sweep_imm4_foisp0.7.RDS")
saveRDS(sweep_int1, "../1_main0.7/0_sweep_imm4_foisp0.7_int.RDS")

sweepkappa_intvax <- readRDS("../2_kappasweep/0_sweep_imm4_foi0.6_sweepkappa_intvax.RDS")
sweepkappa_novax <- readRDS("../2_kappasweep/0_sweep_imm4_foi0.6_sweepkappa_novax.RDS")
sweepkappa_serovax <- readRDS("2_kappasweep/0_sweep_imm4_foi0.6_sweepkappa_vax.RDS")

sweep_intvax <-sweepkappa_intvax%>%filter(sweep_unique<=32)%>%mutate(foi_sp=0.7,foi_sn=1.3)
sweep_novax <- sweepkappa_novax%>%filter(sweep_unique<=32)%>%mutate(foi_sp=0.7,foi_sn=1.3)
sweep_serovax <- sweepkappa_serovax%>%filter(sweep_unique<=32)%>%mutate(foi_sp=0.7,foi_sn=1.3)

sweep_vax <- rbind(sweep_novax,sweep_serovax)

saveRDS(sweep_intvax,"2_kappasweep0.7/0_sweep_imm4_foi0.7_sweepkappa_intvax.RDS")
saveRDS(sweep_vax,"2_kappasweep0.7/0_sweep_imm4_foi0.7_sweepkappa_vax.RDS")

##Additional interval vax scenarios
sweep1 <- readRDS("../3_foisweep/1_sweep_imm4_varyfoi_vax_int.RDS")
##Check
sweep1%>%group_by(kappa3, omega2_pc,foi_sp,vax_first,vax_int)%>%dplyr::summarise(n=n())
sweep1 <- sweep1%>%filter(foi_sp==0.7&kappa3==1/2500)%>%select(-vax_first,-vax_int, -vax_start,-vax_end)%>%unique()

##Start at day 180
sweep_180 <- rbind(sweep1,sweep1)%>%
               mutate(vax_first=rep(180,times=64),
                      vax_int  =rep(c(365,730),each=32))

##Start at day 416
sweep_416 <- rbind(sweep1,sweep1)%>%
              mutate(vax_first=rep(416,times=64),
                     vax_int  =rep(c(365,730),each=32))

##Start at day 519
sweep_590 <- rbind(sweep1,sweep1)%>%
              mutate(vax_first=rep(590,times=64),
                     vax_int  =rep(c(365,730),each=32))

##Start at day 916
sweep_916 <- rbind(sweep1,sweep1)%>%
              mutate(vax_first=rep(916,times=64),
                     vax_int  =rep(c(365,730),each=32))

##Start at day 1703
sweep_1703 <- rbind(sweep1,sweep1)%>%
                    mutate(vax_first=rep(1703,times=64),
                           vax_int  =rep(c(365,730),each=32))

sweep_int <- rbind(sweep_180,sweep_416,sweep_590,sweep_916,sweep_1703)
sweep_int <- sweep_int%>%
                   mutate(vax_start=(vax_first/vax_int)%%1,
                          vax_end  =((vax_first+30)/vax_int)%%1)

add_times <- c(90,270,635,725,995,1100,1200,1300,1400,1500,1600,1800,1900,2000)

saveRDS(sweep_int, "0_sweep_imm4_foisp0.7_intsens.RDS")

sweep_int <- readRDS("0_sweep_imm4_foisp0.7_intsens.RDS")

sweep_int<- rbind(sweep_int,sweep_int,sweep_int)%>%slice(1:896)
sweep_int<- sweep_int%>%
            mutate(vax_first = rep(add_times, times=64))%>%
            mutate(vax_start=(vax_first/vax_int)%%1,
                   vax_end  =((vax_first+30)/vax_int)%%1)

saveRDS(sweep_int, "0_sweep_imm4_foisp0.7_intsens2.RDS")
  
##Interval vax
sweep_int<- rbind(sweep,sweep,sweep,sweep)
sweep_int<- sweep_int%>%
  mutate(vax_first = rep(c(435,800), each=320),
         vax_int   = rep(c(365,   730,   365,   730),each=160),
         vax_start = rep(c((435/365)%%1, 
                           (435/730)%%1, 
                           (800/365)%%1, 
                           (800/730)%%1),each=160),
         
         vax_end   = rep(c((465/365)%%1,
                           (465/730)%%1,
                           (830/365)%%1,
                           (830/730)%%1),each=160))

sweep_int2 <- rbind(sweep,sweep)%>%
  mutate(vax_first = 0,
         vax_int   = rep(c(365,730),         each=160),
         vax_start = rep(c(300/365, 300/730),each=160),
         vax_end   = rep(c(330/365, 330/730),each=160))