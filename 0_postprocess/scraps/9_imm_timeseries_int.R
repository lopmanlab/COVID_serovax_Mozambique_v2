vaxscen <- data.frame(vax_int = c(365,365,365,730,730,730),
                      vax_first=c(0,  435,800,0  ,435,800),
                      scen = c("ann_late","ann_435","ann_800","bi_late","bi_435","bi_800"))

###################
##Compile immune###
####################
##Immunity tiers
imm <- list()


for(i in 1:length(list1)){
  parms <- mod_scen[[list1[i]]]$params
  
  vax_int   <- parms[["vax_int"]]
  vax_first <- parms[["vax_first"]]
  index <- which(vaxscen$vax_int==vax_int&vaxscen$vax_first==vax_first)
  scen <- vaxscen$scen[index]
  
  imm_tmp <- data.frame(time=rep(0, times=3650),
                        date=rep(0, times=3650),
                        sero_thresh=rep("0", times=3650))
  
  imm_tmp$time <- seq(from=1, to =3650, by=1)
  imm_tmp$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  
  #imm_tmp$sweep_unique <- sweep$sweep_unique[list1[i]]
  imm_tmp$r02         <-parms[["r02"]]
  imm_tmp$r03         <-parms[["r03"]]
  imm_tmp$sero_thresh <-scen
  #imm_tmp$sero_thresh =as.character(sweep$sero_thresh[i])
  
  imm_tmp$omega2      <- 1/parms[["omega2_pc"]] 
  imm_tmp$omega4      <- 1/parms[["omega4_pc"]] 
  imm_tmp$kappa       <- 1/parms[["kappa3"]] 
  imm_tmp$foi_sp      <-   parms[["foi_sp"]]
  
  imm_sing <- mod_scen[[list1[i]]]$imm_class%>%filter(time!=0)%>%
    mutate(s0v2_a = s0v1_a+s0v2_a,
           s1v2_a = s1v1_a+s1v2_a,
           s2v2_a = s2v1_a+s2v2_a,
           s0v2_e = s0v1_e+s0v2_e,
           s1v2_e = s1v1_e+s1v2_e,
           s2v2_e = s2v1_e+s2v2_e,
           imm1_c = rec_c+vac_c,
           imm1_a = rec_a+vac_a,
           imm1_e = rec_e+vac_e)%>%
    select(time, s0v0_c, s1v0_c, s2v0_c, 
           s0v0_a, s1v0_a, s2v0_a, s0v2_a, s1v2_a, s2v2_a, s0v3_a, s1v3_a, s2v3_a,
           s0v0_e, s1v0_e, s2v0_e, s0v2_e, s1v2_e, s2v2_e, s0v3_e, s1v3_e, s2v3_e,
           imm1_c:imm1_e)
  imm[[i]]<- imm_tmp %>%left_join(imm_sing, by= c("time"="time"))
  
}

imm<- do.call(rbind, imm)


imm<- imm%>%
  pivot_longer(cols =s0v0_c:imm1_e, names_to = "var", values_to="val")%>%
  mutate(age_grp = substr(var, 6,6),
         imm_cat = substr(var, 1,4),
         imm_cat = factor(imm_cat, levels = c("imm1","s0v0","s1v0","s2v0", "s0v2","s1v2","s2v2", "s0v3","s1v3","s2v3")),
         age_grp = factor(age_grp, levels=c("c","a","e")),
         omega = paste0(omega2,"-",omega4))
#imm<-imm%>%filter(sero_thresh %in% c("0","0.5","0.65","0.8","Annual","Biennial"))

##########################################
###Median value over all Rt estimates#####
##########################################
imm_med<- imm%>%group_by(date,sero_thresh,imm_cat, age_grp, omega,kappa, foi_sp)%>%
  dplyr::summarise(med = median(val))

