sw_randtime <- readRDS("0_sweep_sero.RDS")
sw_imm4_foi0.7<- readRDS("../1_main0.7/0_sweep_imm4_foisp0.7.RDS")

sts<- sw_randtime%>%select(sweep_unique, st0:st9)%>%unique()

sweep <-sw_imm4_foi0.7%>%select(bl:sweep,add_imm:foi_sn)%>%mutate(sero_thresh=0)%>%slice(1:96)

sweep<-cbind(sweep,sts%>%slice(1:96))

sweep <- sweep%>%
          dplyr::rename("sweep_unique_st"="sweep_unique")

sweep2 <- rbind(sweep,sweep,sweep,sweep,sweep,sweep,sweep,sweep)
sweep2 <- sweep2%>%mutate(sero_thresh = rep(c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8),each=96))

sweep2<- sweep2%>%
        dplyr::rename("sweep_unique"="sweep_unique_st")

saveRDS(sweep2,"0_sweep_randtime_serovax.RDS")

##intvax part
sweep3 <-rbind(sweep,sweep)
sweep3<- sweep3%>%
          mutate(vax_first = 0,
                 vax_int   = rep(c(365,730),         each=96),
                 vax_start = rep(c(300/365, 300/730),each=96),
                 vax_end   = rep(c(330/365, 330/730),each=96))
sweep3<- sweep3%>%
          dplyr::rename("sweep_unique"="sweep_unique_st")
saveRDS(sweep3, "0_sweep_randtime_intvax.RDS")
