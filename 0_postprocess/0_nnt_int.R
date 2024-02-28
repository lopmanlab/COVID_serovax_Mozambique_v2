#Create dataframe to hold results for NNT
###NNTs from interval vaccination
  

nnt <- data.frame(
  sweep_unique = rep(0,n),
  num_dose = rep(0,n),    num_campaigns = rep(0,n), firstvax = rep(0,n),
  num_deaths = rep(0,n),  num_deaths_c = rep(0,n),  num_deaths_a = rep(0,n), num_deaths_e = rep(0,n),
  deaths_averted=rep(0,n), nnt = rep(0,n),
  omega2 = rep(0,n), omega4 = rep(0,n), kappa = rep(0,n), foi_sp = rep(0,n)
)


times =data.frame(num1 = seq(from=0, to=9, by=1),
                  num2 = seq(from=0, to=4, by=1))%>%
  mutate(ann_late = 300+365*num1,
         bi_late =  300+730*num2)


vax_times=list()
vax_times[["ann_late"]] <- unique(unlist(times$ann_late))
vax_times[["bi_late"]] <- unique(unlist(times$bi_late))



vax_times[["ann_435"]] <- 435+365*(c(0,seq(1:8)))
vax_times[["ann_800"]] <- 800+365*(c(0,seq(1:7)))
vax_times[["bi_435"]] <-  435+730*(c(0,seq(1:4)))
vax_times[["bi_800"]] <-  800+730*(c(0,seq(1:3)))
vax_times[["ann_180"]]<-  180+365*(c(0,seq(1:9)))
vax_times[["ann_416"]]<-  416+365*(c(0,seq(1:8)))
vax_times[["ann_590"]]<-  590+365*(c(0,seq(1:8)))
vax_times[["ann_916"]]<-  916+365*(c(0,seq(1:7)))
vax_times[["ann_1703"]]<- 1703+365*(c(0,seq(1:5)))
vax_times[["bi_180"]]<-  180+730*(c(0,seq(1:5)))
vax_times[["bi_416"]]<-  416+730*(c(0,seq(1:4)))
vax_times[["bi_590"]]<-  590+730*(c(0,seq(1:4)))
vax_times[["bi_916"]]<-  916+730*(c(0,seq(1:4)))
vax_times[["bi_1703"]]<- 1703+730*(c(0,seq(1:3)))
vax_times[["ann_90"]] <- 90+365*(c(0,seq(1:9)))
vax_times[["ann_270"]]<- 270+365*(c(0,seq(1:9)))
vax_times[["ann_635"]]<- 635+365*(c(0,seq(1:8)))
vax_times[["ann_725"]]<- 725+365*(c(0,seq(1:8)))
vax_times[["ann_995"]]<- 995+365*(c(0,seq(1:7)))
vax_times[["ann_1100"]]<-1100+365*(c(0,seq(1:6)))
vax_times[["ann_1200"]]<-1200+365*(c(0,seq(1:6))) 
vax_times[["ann_1300"]]<-1300+365*(c(0,seq(1:6)))
vax_times[["ann_1400"]]<-1400+365*(c(0,seq(1:6)))
vax_times[["ann_1500"]]<-1500+365*(c(0,seq(1:5)))
vax_times[["ann_1600"]]<-1600+365*(c(0,seq(1:5)))
vax_times[["ann_1800"]]<-1800+365*(c(0,seq(1:5)))
vax_times[["ann_1900"]]<-1900+365*(c(0,seq(1:4)))
vax_times[["ann_2000"]]<-2000+365*(c(0,seq(1:4)))
vax_times[["bi_90"]] <- 90+730*(c(0,seq(1:4)))
vax_times[["bi_270"]]<- 270+730*(c(0,seq(1:4)))
vax_times[["bi_635"]]<- 635+730*(c(0,seq(1:4)))
vax_times[["bi_725"]]<- 725+730*(c(0,seq(1:4)))
vax_times[["bi_995"]]<- 995+730*(c(0,seq(1:3)))
vax_times[["bi_1100"]]<-1100+730*(c(0,seq(1:3)))
vax_times[["bi_1200"]]<-1200+730*(c(0,seq(1:3))) 
vax_times[["bi_1300"]]<-1300+730*(c(0,seq(1:3)))
vax_times[["bi_1400"]]<-1400+730*(c(0,seq(1:3)))
vax_times[["bi_1500"]]<-1500+730*(c(0,seq(1:2)))
vax_times[["bi_1600"]]<-1600+730*(c(0,seq(1:2)))
vax_times[["bi_1800"]]<-1800+730*(c(0,seq(1:2)))
vax_times[["bi_1900"]]<-1900+730*(c(0,seq(1:2)))
vax_times[["bi_2000"]]<-2000+730*(c(0,seq(1:2)))



vaxscen <- data.frame(vax_int = c(rep(365,times =22),
                                  rep(730,times=22)),
                      vax_first=c(0,  435,800,180,416,590,916,1703,
                                  90,270,635,725,995,1100,1200,1300,1400,1500,1600,1800,1900,2000,
                                  0  ,435,800,180,416,590,916,1703,
                                  90,270,635,725,995,1100,1200,1300,1400,1500,1600,1800,1900,2000),
                      scen = c("ann_late","ann_435","ann_800","ann_180","ann_416","ann_590","ann_916","ann_1703",
                               "ann_90","ann_270","ann_635","ann_725","ann_995","ann_1100","ann_1200","ann_1300",
                               "ann_1400","ann_1500","ann_1600","ann_1800","ann_1900","ann_2000",
                               "bi_late", "bi_435", "bi_800", "bi_180", "bi_416",  "bi_590", "bi_916", "bi_1703",
                               "bi_90","bi_270","bi_635","bi_725","bi_995","bi_1100","bi_1200","bi_1300",
                               "bi_1400","bi_1500","bi_1600","bi_1800","bi_1900","bi_2000"))
#vax_int_all <- rep(unique(sweep$vax_int),each=14)
#vax_first_all <-rep(unique(sweep$vax_first),times=2)

#scen<- list()
#for(i in 1:length(vax_first_all)){
#  vax_int <- vax_int_all[i]
#  vax_first<- vax_first_all[i]
#  index <- which(vaxscen$vax_int==vax_int&vaxscen$vax_first==vax_first)
#  scen[i] <- vaxscen$scen[index]
#}


##NUmber of vaccine doses administered
##Need to get the number vaccine eligible at the start of the vax rounds and then multiply by exponential Pert thing for each round

for(i in 1:n){
  ## get the vax time scenario from scenario runs list
  parms <- mod_scen[[i]]$params   ##Get the params from the model run
  
  ##Get the vaccination scenario for interval vaccination based on start time and interval
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
  
  #nnt$r02[i]          <- parms[["r02"]]
  #nnt$r03[i]          <- parms[["r03"]]
  #nnt$sero_thresh[i]  <- as.character(parms[["sero_thresh"]])
  nnt$sweep_unique[i] <- parms[["sweep_unique"]]
  nnt$sero_thresh[i]  <- scen
  nnt$omega2[i]       <- 1/parms[["omega2_pc"]]
  nnt$omega4[i]       <- 1/parms[["omega4_pc"]]
  nnt$kappa[i]        <- 1/parms[["kappa3"]]
  nnt$foi_sp[i]       <- parms[["foi_sp"]]
  #nnt$vax_first[i]    <- parms[["vax_first"]]
  #nnt$vax_int[i]      <- parms[["vax_int"]]
}


