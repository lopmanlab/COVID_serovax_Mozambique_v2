#Create dataframe to hold results for NNT
nnt <- data.frame(
  sweep_unique = rep(0,n),sero_thresh = rep("0",n), 
  num_dose = rep(0,n),    num_campaigns = rep(0,n), firstvax = rep(0,n),
  num_deaths = rep(0,n),  num_deaths_c = rep(0,n),  num_deaths_a = rep(0,n), num_deaths_e = rep(0,n),
  deaths_averted=rep(0,n), nnt = rep(0,n),
  omega2 = rep(0,n), omega4 = rep(0,n), kappa = rep(0,n), foi_sp = rep(0,n)
)

for(i in 1:n){
  parms <- mod_scen[[i]]$params
  
  ##NUmber of vaccine doses administered
  vax_dose       <- mod_scen[[i]]$vax_elig[1:3650,] %>%
    mutate(flag = ifelse(delta3_er == 0.02 & lag(delta3_er==0),1,0))%>%
    filter(flag==1)%>%
    mutate(est_vax_doses = vax_elig_e - vax_elig_e*2.718^(-delta3_er*30))
  
  nnt$num_dose[i]<-       sum(vax_dose$est_vax_doses)
  nnt$num_campaigns[i] <- nrow(vax_dose)
  nnt$firstvax[i] <-      vax_dose$time[1]
  
  ##Number of deaths
  nnt$num_deaths[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_tot[1:3650], na.rm=T)
  nnt$num_deaths_c[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_c[1:3650], na.rm=T)
  nnt$num_deaths_a[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_a[1:3650], na.rm=T)
  nnt$num_deaths_e[[i]] <- sum(mod_scen[[i]]$pop_num$new_Deaths_e[1:3650], na.rm=T)
  
  ##Extracting metadata from the params
  #nnt$sweep_unique[i] <- sweep$sweep_unique[i]
  nnt$r02[i]          <- parms[["r02"]]
  nnt$r03[i]          <- parms[["r03"]]

  
  nnt$sero_thresh[i]  <- as.character(parms[["sero_thresh"]])
  nnt$omega2[i]       <- 1/parms[["omega2_pc"]]
  nnt$omega4[i]       <- 1/parms[["omega4_pc"]]
  nnt$kappa[i]        <- 1/parms[["kappa3"]]
  nnt$foi_sp[i]       <- parms[["foi_sp"]]
}