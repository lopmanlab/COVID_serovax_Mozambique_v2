#Create dataframe to hold results for NNT

nnt <- data.frame(
  sweep_unique = rep(0,n),
  num_dose = rep(0,n),    num_campaigns = rep(0,n), firstvax = rep(0,n),
  num_deaths = rep(0,n),  num_deaths_c = rep(0,n),  num_deaths_a = rep(0,n), num_deaths_e = rep(0,n),
  deaths_averted=rep(0,n), nnt = rep(0,n),
  omega2 = rep(0,n), omega4 = rep(0,n), kappa = rep(0,n), foi_sp = rep(0,n)
)


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

for(i in 1:n){
  ## get the vax time scenario from scenario runs list
  parms <- mod_scen[[i]]$params   ##Get the params from the model run
  vax_int   <- parms[["vax_int"]]
  vax_first <- parms[["vax_first"]]
  
  index <- which(vaxscen$vax_int==vax_int&vaxscen$vax_first==vax_first)
  scen <- vaxscen$scen[index]
  
  vax_times_vec <- vax_times[[which(names(vax_times)== scen)]]
  delta3_er <- 0.02
  
  vax_dose <- mod_scen[[i]]$vax_elig[1:3650,] %>%
    select(time:vax_elig_a)%>%
    filter(time %in% unlist(vax_times_vec))%>%
    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
  nnt$num_dose[i]<- sum(vax_dose$est_vax_doses)
  nnt$num_campaigns[i] <- nrow(vax_dose)
  nnt$firstvax[i] <- vax_dose$time[1]
  
  
  nnt$num_deaths[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_tot[1:3650], na.rm=T)
  #post2 <- mod_scen_int[[i]]$pop_num %>%filter(time>730)
  nnt$num_deaths_c[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_c[1:3650], na.rm=T)
  nnt$num_deaths_a[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_a[1:3650], na.rm=T)
  nnt$num_deaths_e[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_e[1:3650], na.rm=T)
  

  #nnt$sweep_unique[i] <- sweep$sweep_unique[i]
  nnt$r02[i]          <- parms[["r02"]]
  nnt$r03[i]          <- parms[["r03"]]
  nnt$sero_thresh[i]  <- scen
  nnt$omega2[i]       <- 1/parms[["omega2_pc"]]
  nnt$omega4[i]       <- 1/parms[["omega4_pc"]]
  nnt$kappa[i]        <- 1/parms[["kappa3"]]
  nnt$foi_sp[i]       <- parms[["foi_sp"]]
  nnt$vax_first[i]    <- parms[["vax_first"]]
  nnt$vax_int[i]      <- parms[["vax_int"]]
}



