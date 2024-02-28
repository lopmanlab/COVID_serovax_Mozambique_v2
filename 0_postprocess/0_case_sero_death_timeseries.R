###################################
####Time series cases per 100######
##################################
inc <- list()

for(i in 1:n){
  parms <- mod_scen[[i]]$params 
  
  inc[[i]]<- mod_scen[[i]]$mod_inc[2:3651,]%>%
    select(time, new_I_c, new_I_a, new_I_e, new_I)%>%
    
    mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
           #sweep_unique = sweep$sweep_unique[i],
           #sero_thresh =as.character(sweep$sero_thresh[i]),
           sweep_unique=parms[["sweep_unique"]],
           sero_thresh = as.character(parms[["sero_thresh"]]),
           omega2     =1/parms[["omega2_pc"]],
           omega4     =1/parms[["omega4_pc"]],
           kappa      =1/parms[["kappa3"]],
           foi_sp     =  parms[["foi_sp"]])
}

##Combine into one dataframe
inc<- do.call(rbind, inc)

##Rename to match original
inc<- inc%>%
      dplyr::rename("inc_c"  ="new_I_c",
             "inc_a"  ="new_I_a",
             "inc_e"  ="new_I_e",
             "inc_all"="new_I")

#################
## Compile sero##
##################

sero <- list()

##Get serothresh piece
for(i in 1:n){
  parms <- mod_scen[[i]]$params
  
  sero[[i]]<- mod_scen[[i]]$seroprev[2:3651,] %>%
    select(time, seroprev_c, seroprev_a, seroprev_e, seroprev_to)%>%
    
    mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
           #sweep_unique = sweep$sweep_unique[i],
           #sero_thresh =as.character(sweep$sero_thresh[i]),           
           sweep_unique= parms[["sweep_unique"]],
           sero_thresh = as.character(parms[["sero_thresh"]]),
           omega2     =1/parms[["omega2_pc"]],
           omega4     =1/parms[["omega4_pc"]],
           kappa      =1/parms[["kappa3"]],
           foi_sp     =  parms[["foi_sp"]])
}

###Combine into one dataframe
sero<- do.call(rbind, sero)
##Rename to match original
sero<- sero%>%
        dplyr::rename("sero_c"  ="seroprev_c",
               "sero_a"  ="seroprev_a",
               "sero_e"  ="seroprev_e",
               "sero_all"="seroprev_to")


#################
###Deaths######
################

deaths <- list()

for(i in 1:n){
  parms <- mod_scen[[i]]$params
  
  deaths[[i]]<- mod_scen[[i]]$pop_num[2:3651,]%>% 
    select(time, new_Deaths_c:new_Deaths_tot)%>%
    mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
           #sweep_unique = sweep$sweep_unique[i],
           #sero_thresh =as.character(sweep$sero_thresh[i]), 
           sweep_unique= parms[["sweep_unique"]],
           sero_thresh = as.character(parms[["sero_thresh"]]),
           omega2     =1/parms[["omega2_pc"]],
           omega4     =1/parms[["omega4_pc"]],
           kappa      =1/parms[["kappa3"]],
           foi_sp     =  parms[["foi_sp"]])
}

deaths<- do.call(rbind, deaths)


