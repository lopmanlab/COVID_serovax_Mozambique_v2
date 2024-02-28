vaxscen <- data.frame(vax_int = c(rep(365,times =8),
                                  rep(730,times=8)),
                      vax_first=c(0,  435,800,180,416,590,916,1703,
                                  0  ,435,800,180,416,590,916,1703),
                      scen = c("ann_late","ann_435","ann_800","ann_180","ann_416","ann_590","ann_916","ann_1703",
                               "bi_late", "bi_435", "bi_800", "bi_180", "bi_416",  "bi_590", "bi_916", "bi_1703"))


inc <- list()
sero <- list()
deaths<- list()

for(i in 1:n){
  
  parms <- mod_scen[[i]]$params 
  ##Get the vaccination scenario for interval vaccination based on start time and interval
  vax_int   <- parms[["vax_int"]]
  vax_first <- parms[["vax_first"]]
  index <- which(vaxscen$vax_int==vax_int&vaxscen$vax_first==vax_first)
  scen <- vaxscen$scen[index]
  
  ###################################
  ####Time series cases per 100######
  ##################################
  inc[[i]]<- mod_scen[[i]]$mod_inc[2:3651,]%>%
              select(time, new_I_c, new_I_a, new_I_e, new_I)%>%
            
              mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
                     sweep_unique=parms[["sweep_unique"]],
                     sero_thresh = as.character(scen),
                     omega2     =1/parms[["omega2_pc"]],
                     omega4     =1/parms[["omega4_pc"]],
                     kappa      =1/parms[["kappa3"]],
                     foi_sp     =  parms[["foi_sp"]])
  
  
  ##################
  ##Seroprevalence##
  ##################
  sero[[i]]<- mod_scen[[i]]$seroprev[2:3651,] %>%
                select(time, seroprev_c, seroprev_a, seroprev_e, seroprev_to)%>%
                
                mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
                       sweep_unique= parms[["sweep_unique"]],
                       sero_thresh = as.character(scen),
                       omega2     =1/parms[["omega2_pc"]],
                       omega4     =1/parms[["omega4_pc"]],
                       kappa      =1/parms[["kappa3"]],
                       foi_sp     =  parms[["foi_sp"]])
  
  ################
  ###Deaths#######
  ################
  deaths[[i]]<- mod_scen[[i]]$pop_num[2:3651,]%>% 
                select(time, new_Deaths_c:new_Deaths_tot)%>%
                mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
                       sweep_unique= parms[["sweep_unique"]],
                       sero_thresh = as.character(scen),
                       omega2     =1/parms[["omega2_pc"]],
                       omega4     =1/parms[["omega4_pc"]],
                       kappa      =1/parms[["kappa3"]],
                       foi_sp     =  parms[["foi_sp"]])
}

##Combine inc into one dataframe
inc<- do.call(rbind, inc)
##Rename to match original
inc<- inc%>%
      dplyr::rename("inc_c"  ="new_I_c",
             "inc_a"  ="new_I_a",
             "inc_e"  ="new_I_e",
             "inc_all"="new_I")

##Combine sero into one dataframe
sero<- do.call(rbind, sero)
##Rename to match original
sero<- sero%>%
        dplyr::rename("sero_c"  ="seroprev_c",
               "sero_a"  ="seroprev_a",
               "sero_e"  ="seroprev_e",
               "sero_all"="seroprev_to")

##Combine deaths into one dataframe
deaths<- do.call(rbind, deaths)


