sweep0<-readRDS("1_main/0_sweep_sero.RDS")
sweep<- readRDS("1_main/0_sweep_imm4.RDS")
sweep_r0 <- sweep0%>%group_by(sweep_unique)%>%slice(1)

sweep<- sweep%>%
        filter(omega2_pc==1/180)%>%
        filter(kappa3%in%c(1/1000,1/1500,1/2000,1/2500))%>%
        filter(sero_thresh==0)

sweep <- rbind(sweep,sweep,sweep,sweep,sweep)

sweep <- sweep%>%mutate(
         foi_sp = rep(c(0.5,0.6,0.7,0.8, 0.9), each =128),
         foi_sn = rep(c(1.5,1.4,1.3,1.2,1.0), each =128)
)

saveRDS(sweep,"0_sweep_imm4_varyfoi.RDS")

sweep0<- sweep%>%filter(kappa3 ==1/2500)%>%
          filter(foi_sp==0.5)

sweep0.5 <-rbind(sweep0, sweep0,sweep0,sweep0,sweep0, sweep0,sweep0)

sweep0.5<- sweep0.5%>%
           mutate(sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=32))

sweep0.6 <-rbind(sweep0, sweep0,sweep0,sweep0,sweep0, sweep0,sweep0)

sweep0.6<- sweep0.6%>%
  mutate(foi_sp = 0.6, foi_sn=1.4,
         sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=32))

sweep0.7 <-rbind(sweep0, sweep0,sweep0,sweep0,sweep0, sweep0,sweep0)

sweep0.7<- sweep0.7%>%
  mutate(foi_sp = 0.7, foi_sn = 1.3,
           sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=32))


sweep0.8 <-rbind(sweep0, sweep0,sweep0,sweep0,sweep0, sweep0,sweep0)

sweep0.8<- sweep0.8%>%
          mutate(foi_sp=0.8, foi_sn=1.2,
                sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=32))


sweep0.9 <-rbind(sweep0, sweep0,sweep0,sweep0,sweep0, sweep0,sweep0)

sweep0.9<- sweep0.9%>%
          mutate(foi_sp=0.9, foi_sn=1.1,
                sero_thresh = rep(c(0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=32))


sweep_vax <- rbind(sweep0.5,sweep0.6,sweep0.7,sweep0.8,sweep0.9)
saveRDS(sweep_vax,"0_sweep_imm4_varyfoi_vax.RDS")


sweep_vax <- readRDS("1_main/0_sweep_imm4_varyfoi_vax.RDS")
sweep_vax0<- sweep_vax%>%
              filter(foi_sp %in% c(0.5,0.6))%>%
              filter(sero_thresh==0.5)%>%
              mutate(sero_thresh=0)

sweep<- rbind(sweep_vax0,sweep_vax0, sweep_vax0, sweep_vax0)

sweep<- sweep%>%
  mutate(vax_first = rep(c(435,800), each=128),
         vax_int   = rep(c(365,   730,   365,   730),each=64),
         vax_start = rep(c((435/365)%%1, 
                           (435/730)%%1, 
                           (800/365)%%1, 
                           (800/730)%%1),each=64),
         
         vax_end   = rep(c((465/365)%%1,
                           (465/730)%%1,
                           (830/365)%%1,
                           (830/730)%%1),each=64))
saveRDS(sweep, "1_sweep_imm4_varyfoi_vax_int_pt1.RDS")


sweep_vax <- readRDS("1_main/0_sweep_imm4_varyfoi_vax.RDS")
sweep_vax0<- sweep_vax%>%
  filter(foi_sp %in% c(0.7,0.8,0.9))%>%
  filter(sero_thresh==0.5)%>%
  mutate(sero_thresh=0)

sweep<- rbind(sweep_vax0,sweep_vax0, sweep_vax0, sweep_vax0)

sweep<- sweep%>%
  mutate(vax_first = rep(c(435,800), each=192),
         vax_int   = rep(c(365,   730,   365,   730),each=96),
         vax_start = rep(c((435/365)%%1, 
                           (435/730)%%1, 
                           (800/365)%%1, 
                           (800/730)%%1),each=96),
         
         vax_end   = rep(c((465/365)%%1,
                           (465/730)%%1,
                           (830/365)%%1,
                           (830/730)%%1),each=96))

saveRDS(sweep, "1_main/1_sweep_imm4_varyfoi_vax_int_pt2.RDS")

sweep_vax <- readRDS("1_main/0_sweep_imm4_varyfoi_vax.RDS")
sweep_vax0<- sweep_vax%>%
  filter(foi_sp %in% c(0.5,0.6,0.7,0.8,0.9))%>%
  filter(sero_thresh==0.5)%>%
  mutate(sero_thresh=0)

sweep_int <- rbind(sweep_vax0,sweep_vax0)%>%
  mutate(vax_first = 0,
         vax_int   = rep(c(365,730),         each=160),
         vax_start = rep(c(300/365, 300/730),each=160),
         vax_end   = rep(c(330/365, 330/730),each=160))
saveRDS(sweep_int, "1_main/1_sweep_imm4_varyfoi_vax_int_pt3.RDS")

sweep_int<- sweep_int%>%arrange(sweep_unique)

sweep_int_pt1<- readRDS("1_main/1_sweep_imm4_varyfoi_vax_int_pt1.RDS")

###200 runs of kappa 2500, foi_sp0.6

sweep0<-readRDS("1_main/0_sweep_sero.RDS")
sweep<- readRDS("1_main/0_sweep_imm4.RDS")
sweep_r0 <- sweep0%>%group_by(sweep_unique)%>%slice(1)
sweep_r0 <- sweep_r0%>%filter(sweep_unique>32&sweep_unique<=192)

sweep<- sweep%>%
  filter(omega2_pc==1/180)%>%
  filter(kappa3==1/2500)%>%
  filter(sero_thresh==0)

sweep <- rbind(sweep,sweep,sweep,sweep,sweep)
sweep <- sweep%>%
          mutate(r00=sweep_r0$r00,
                 r01=sweep_r0$r01,
                 r02=sweep_r0$r02,
                 r03=sweep_r0$r03,
                 r04=sweep_r0$r04,
                 r05=sweep_r0$r05,
                 r06=sweep_r0$r06,
                 r07=sweep_r0$r07,
                 r08=sweep_r0$r08,
                 r09=sweep_r0$r09,
                 sweep_unique = sweep_r0$sweep_unique)

sweep <- sweep%>%mutate(
            foi_sp = 0.6,
            foi_sn = 1.4)

##Now add the different serothresh
sweep_thresh <- rbind(sweep,sweep,sweep,sweep,sweep,sweep,sweep,sweep)
sweep_thresh <- sweep_thresh%>%
                mutate(sero_thresh = rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8), each=160))

saveRDS(sweep_thresh, "1_main/0_sweep_imm4_foisp0.6.RDS")

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

sweep_int <- rbind(sweep_int, sweep_int2)
saveRDS(sweep_int, "1_main/0_sweep_imm4_foisp0.6_int.RDS")

##100 runs Sweep kappa for foi_sp=0.6
sweep0<-readRDS("1_main/0_sweep_sero.RDS")
sweep<- readRDS("1_main/0_sweep_imm4.RDS")
sweep_r0 <- sweep0%>%group_by(sweep_unique)%>%slice(1)
sweep_r0 <- sweep_r0%>%filter(sweep_unique<=96)

sweep<- sweep%>%
  filter(omega2_pc==1/180)%>%
  filter(kappa3==1/500)%>%
  filter(sero_thresh==0)

sweep <- rbind(sweep,sweep,sweep)
sweep <- sweep%>%
  mutate(r00=sweep_r0$r00,
         r01=sweep_r0$r01,
         r02=sweep_r0$r02,
         r03=sweep_r0$r03,
         r04=sweep_r0$r04,
         r05=sweep_r0$r05,
         r06=sweep_r0$r06,
         r07=sweep_r0$r07,
         r08=sweep_r0$r08,
         r09=sweep_r0$r09,
         sweep_unique = sweep_r0$sweep_unique)

sweep <- sweep%>%mutate(
  foi_sp = 0.6,
  foi_sn = 1.4)

##Now add the different serothresh
sweep_thresh <- rbind(sweep,sweep,sweep,sweep)
sweep_thresh <- sweep_thresh%>%
  mutate(sero_thresh = rep(c(0.5,0.6,0.7,0.8), each=96))

sweep_kappa <- rbind(sweep_thresh,sweep_thresh,sweep_thresh,
                     sweep_thresh, sweep_thresh,sweep_thresh)
sweep_kappa <- sweep_kappa%>%
                mutate(kappa2 = rep(c(1/500,1/1000,1/1500,1/2000,1/3000,1/3500), each=384),
                       kappa3 = rep(c(1/500,1/1000,1/1500,1/2000,1/3000,1/3500), each=384))
saveRDS(sweep_kappa, "0_sweep_imm4_foi0.6_sweepkappa_vax.RDS")

##Need vaxed scenarios for the sweepkappa....
###Redo foi_sp no vax 0.9....

sweep<- readRDS("3_foisweep/0_sweep_imm4_varyfoi_novax.RDS")
sweep<- sweep%>%filter(foi_sp==0.9)
sweep<- sweep%>%mutate(foi_sn=1.1)
saveRDS(sweep,"3_foisweep/0_sweep_imm4_varyfoi_foisp0.9_redo.RDS")



