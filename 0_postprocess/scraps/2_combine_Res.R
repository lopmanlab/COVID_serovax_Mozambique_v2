library(dplyr)
library(deSolve)
library(ggplot2)
library(tidyr)
#####Read in mod_scen for serologically triggered scenarios and no vax scenario
#setwd("/projects/blopman/vger/cliu/00_sens/0_immgrid4")

#mod_scen<- readRDS("mod_scen0.RDS")
mod_scen1 <- readRDS("0_res/mod_scen1.RDS")
mod_scen2 <- readRDS("0_res/mod_scen2.RDS")
mod_scen3 <- readRDS("0_res/mod_scen3.RDS")
mod_scen4 <- readRDS("0_res/mod_scen4.RDS")

mod_scen <- c(mod_scen1,mod_scen2,mod_scen3,mod_scen4)

#sweep<- readRDS("0_sweep_imm.RDS")[481:576,]
sweep<- readRDS("0_sweep_imm4_varyfoi.RDS")%>%
  mutate(sero_thresh=as.character(sero_thresh))%>%
  slice(1:384)


imm_grid <- data.frame(
  v0=c(1,               sweep$rel_1v0[1],sweep$rel_2v0[1]),
  v1=c(sweep$rel_0v1[1],sweep$rel_1v1[1],sweep$rel_2v1[1]),
  v2=c(sweep$rel_0v2[1],sweep$rel_1v2[1],sweep$rel_2v2[1]),
  v3=c(sweep$rel_0v3[1],sweep$rel_1v3[1],sweep$rel_2v3[1]))
row.names(imm_grid)=c("exp0","exp1","exp2")

imm_vec <- imm_grid%>%
  mutate(exp = row.names(.))%>%
  pivot_longer(cols=v0:v3, names_to="vax",values_to = "val")%>%
  arrange(vax,exp)

imm_vec <- unlist(imm_vec$val)

##Population distribution
tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop
pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)
old_pop <-  992149+446454


n <- nrow(sweep)
###################################
####Time series cases per 100######
##################################
inc <- list()


for(i in 1:n){
  inc[[i]] <- data.frame(val=rep(0,times=3650),
                         time=rep(0, times=3650),
                         date=rep(0, times=3650),
                         sweep_unique=rep(0,times=3650),
                         sero_thresh=rep(0, times=3650))
  inc[[i]]$inc_c<- (mod_scen[[i]]$mod_inc[,"new_I_c"][2:3651]/tot_pop)*100
  inc[[i]]$inc_a<- (mod_scen[[i]]$mod_inc[,"new_I_a"][2:3651]/tot_pop)*100
  inc[[i]]$inc_e<- (mod_scen[[i]]$mod_inc[,"new_I_e"][2:3651]/tot_pop)*100
  inc[[i]]$inc_all<- (mod_scen[[i]]$mod_inc[,"new_I"][2:3651]/tot_pop)*100
  inc[[i]]$time <- seq(from=1, to =3650, by=1)
  inc[[i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  inc[[i]]$sweep_unique <- sweep$sweep_unique[i]
  inc[[i]]$sero_thresh <- sweep$sero_thresh[i]  
  inc[[i]]$omega2 <- 1/sweep$omega2_pc[i]
  inc[[i]]$omega4 <-1/sweep$omega4_pc[i]
  inc[[i]]$kappa  <- 1/sweep$kappa2[i]
  inc[[i]]$foi_sp <- sweep$foi_sp[i]
  
}


#saveRDS(inc, "inc_list.RDS")

inc<- do.call(rbind, inc)


#################
## Compile sero##
##################
sero <- list()
##serothresh piece
for(i in 1:n){
  sero[[i]] <- data.frame(sero_c=rep(0,times=3650),
                          sero_a=rep(0,times=3650),
                          sero_e=rep(0,times=3650),
                          time=rep(0, times=3650),
                          date=rep(0, times=3650),
                          sweep_unique=rep(0,times=3650),
                          sero_thresh=rep(0, times=3650))
  sero[[i]]$sero_c<- (mod_scen[[i]]$seroprev[,"seroprev_c"][2:3651])*100
  sero[[i]]$sero_a<- (mod_scen[[i]]$seroprev[,"seroprev_a"][2:3651])*100
  sero[[i]]$sero_e<- (mod_scen[[i]]$seroprev[,"seroprev_e"][2:3651])*100
  sero[[i]]$sero_all <- (mod_scen[[i]]$seroprev[,"seroprev_to"][2:3651])*100
  sero[[i]]$time <- seq(from=1, to =3650, by=1)
  sero[[i]]$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  sero[[i]]$sweep_unique <- sweep$sweep_unique[i]
  sero[[i]]$sero_thresh <- sweep$sero_thresh[i]
  sero[[i]]$omega2 <- 1/sweep$omega2_pc[i]
  sero[[i]]$omega4 <-1/sweep$omega4_pc[i]
  sero[[i]]$kappa  <- 1/sweep$kappa2[i]
  sero[[i]]$foi_sp <- sweep$foi_sp[i]
}



sero<- do.call(rbind, sero)

##Immunity tiers
imm <- list()
#list1 <- which(sweep$sero_thresh %in% c(0, 0.5,0.6,0.7,0.8))
list1 <- which(sweep$sero_thresh %in% c(0))

for(i in 1:length(list1)){
  imm_tmp <- data.frame(time=rep(0, times=3650),
                        date=rep(0, times=3650),
                        sweep_unique=rep(0,times=3650),
                        sero_thresh=rep(0, times=3650))
  
  imm_tmp$time <- seq(from=1, to =3650, by=1)
  imm_tmp$date <- seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1)
  imm_tmp$sweep_unique <- sweep$sweep_unique[list1[i]]
  imm_tmp$sero_thresh <- sweep$sero_thresh[list1[i]] 
  imm_tmp$omega2      <- 1/sweep$omega2_pc[list1[i]] 
  imm_tmp$omega4      <- 1/sweep$omega4_pc[list1[i]]
  imm_tmp$kappa       <- 1/sweep$kappa2[list1[i]]
  imm_tmp$foi_sp      <- sweep$foi_sp[list1[i]]
  
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

imm_med<- imm%>%group_by(date,sero_thresh,imm_cat, age_grp, omega)%>%
  dplyr::summarise(med = median(val))




deaths <- list()

#mod_scen_int<-list()
#mod_scen_int[[1]]<-sw_run_1000
##serothresh piece

for(i in 1:n){
  deaths[[i]]<- mod_scen[[i]]$pop_num[2:3651,]%>% 
    select(time, new_Deaths_c:new_Deaths_tot)%>%
    mutate(date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
           sweep_unique = sweep$sweep_unique[i],
           sero_thresh = sweep$sero_thresh[i],
           omega2     =1/sweep$omega2_pc[i],
           omega4     =1/sweep$omega4_pc[i],
           kappa      =1/sweep$kappa2[i],
           foi_sp     =sweep$foi_sp[i])
  
}

deaths<- do.call(rbind, deaths)

#####################
###Make df_corr###
##################

inc_time <- data.frame(sn = seq(from=0,to=9, by=1),
                       wave= seq(from=1,to=10, by=1))%>%
  mutate(time= sn*365+199)

age_dist <- data.frame(age = c("c","a","e"),
                       pop = c(10884513+4883969, 7958844+4900719, 992149+446454))



## deaths per wave
death_wave <- deaths%>%
  mutate(omega = paste0(omega2,"-",omega4))%>%
  mutate(
    wave = case_when(time<inc_time$time[1]~NA_real_,
                     time<inc_time$time[2]~1,
                     time<inc_time$time[3]~2,
                     time<inc_time$time[4]~3,
                     time<inc_time$time[5]~4,
                     time<inc_time$time[6]~5,
                     time<inc_time$time[7]~6,
                     time<inc_time$time[8]~7,
                     time<inc_time$time[9]~8,
                     time<inc_time$time[10]~9,
                     TRUE~10)
  )%>%
  group_by(sweep_unique, sero_thresh, wave, omega, foi_sp)%>%
  dplyr::summarise(c = sum(new_Deaths_c),
                   a = sum(new_Deaths_a),
                   e = sum(new_Deaths_e),
                   all=sum(new_Deaths_tot)
  )
death_wave_long<- death_wave%>%pivot_longer(cols=c:all, names_to="age", values_to = "tot_death")


##seroprev per wave
sero_wave <- sero%>%
  mutate(omega = paste0(omega2,"-",omega4))%>%
  mutate(
    wave = case_when(time<inc_time$time[1]~NA_real_,
                     time<inc_time$time[2]~1,
                     time<inc_time$time[3]~2,
                     time<inc_time$time[4]~3,
                     time<inc_time$time[5]~4,
                     time<inc_time$time[6]~5,
                     time<inc_time$time[7]~6,
                     time<inc_time$time[8]~7,
                     time<inc_time$time[9]~8,
                     time<inc_time$time[10]~9,
                     TRUE~10)
  )%>%
  group_by(sweep_unique, sero_thresh, wave, omega, foi_sp)%>%
  slice(1)

sero_wave_long <- sero_wave%>%pivot_longer(cols=sero_c:sero_e, names_to="age",values_to = "sero")%>%
  mutate(age = substr(age,6,6))


## immunity per wave
##Outputs
#saveRDS(inc, "3_inc_facet_imm4.RDS")
#saveRDS(sero, "3_sero_facet_imm4.RDS")
#saveRDS(imm, "3_imm_facet_imm4.RDS")
#saveRDS(imm_med, "3_imm_facet_med_imm4.RDS")
#saveRDS(deaths, "3_death_facet_imm4.RDS")


#####################
###Make df_corr###
##################

inc_time <- data.frame(sn = seq(from=0,to=9, by=1),
                       wave= seq(from=1,to=10, by=1))%>%
  mutate(time= sn*365+199)

age_dist <- data.frame(age = c("c","a","e"),
                       pop = c(10884513+4883969, 7958844+4900719, 992149+446454))



## deaths per wave
death_wave <- deaths%>%
  mutate(omega = paste0(omega2,"-",omega4))%>%
  mutate(
    wave = case_when(time<inc_time$time[1]~NA_real_,
                     time<inc_time$time[2]~1,
                     time<inc_time$time[3]~2,
                     time<inc_time$time[4]~3,
                     time<inc_time$time[5]~4,
                     time<inc_time$time[6]~5,
                     time<inc_time$time[7]~6,
                     time<inc_time$time[8]~7,
                     time<inc_time$time[9]~8,
                     time<inc_time$time[10]~9,
                     TRUE~10)
  )%>%
  group_by(sweep_unique, sero_thresh, wave, omega,kappa, foi_sp)%>%
  dplyr::summarise(c = sum(new_Deaths_c),
                   a = sum(new_Deaths_a),
                   e = sum(new_Deaths_e),
                   all=sum(new_Deaths_tot)
  )
death_wave_long<- death_wave%>%pivot_longer(cols=c:all, names_to="age", values_to = "tot_death")


##seroprev per wave
sero_wave <- sero%>%
  mutate(omega = paste0(omega2,"-",omega4))%>%
  mutate(
    wave = case_when(time<inc_time$time[1]~NA_real_,
                     time<inc_time$time[2]~1,
                     time<inc_time$time[3]~2,
                     time<inc_time$time[4]~3,
                     time<inc_time$time[5]~4,
                     time<inc_time$time[6]~5,
                     time<inc_time$time[7]~6,
                     time<inc_time$time[8]~7,
                     time<inc_time$time[9]~8,
                     time<inc_time$time[10]~9,
                     TRUE~10)
  )%>%
  group_by(sweep_unique, sero_thresh, wave, omega,kappa, foi_sp)%>%
  slice(1)

sero_wave_long <- sero_wave%>%pivot_longer(cols=c(sero_c:sero_e,sero_all), names_to="age",values_to = "sero")%>%
  mutate(age = gsub("sero_","",age))


## immunity per wave


#sus_val <- data.frame(imm_cat = unique(imm$imm_cat),
#                      sus_val = c(1,0.54,0.23,0.62,0.33,0.14,0.46,0.25,0.1,0))

sus_val <- data.frame(imm_cat=unique(imm$imm_cat),
                      sus_val = c(imm_vec[1:3],imm_vec[7:12],0))

imm_wave <- imm %>% filter(time %in% unlist(inc_time$time))
imm_wave <- imm_wave%>% left_join(
  sus_val, by = c("imm_cat"="imm_cat")
)


imm_wave1 <- imm_wave%>%
  select(-sus_val, -var)%>%
  tidyr::pivot_wider(names_from = "imm_cat", values_from="val")%>% 
  unchop(everything())%>%
  replace_na(list(s0v2 = 0, s1v2=0, s2v2=0, s0v3=0, s1v3=0, s2v3=0))%>%
  mutate(
    sus_tot = s0v0+s1v0+s2v0+s0v2+s1v2+s2v2+s0v3+s1v3+s2v3) %>% 
  left_join(age_dist, by = c("age_grp"="age"))%>%
  mutate(tot = sus_tot+imm1)



imm_wave2<- imm_wave %>% mutate(
  sus_pop = val*sus_val
)%>%
  select(-sus_val, -val,-var)%>%
  replace_na(list(sus_pop = 0))%>%
  pivot_wider(names_from = "imm_cat", values_from="sus_pop")%>% 
  unchop(everything())%>%
  replace_na(list(s0v2 = 0, s1v2=0, s2v2=0, s0v3=0, s1v3=0, s2v3=0))%>%
  
  mutate(
    sus_tot = s0v0+s1v0+s2v0+s0v2+s1v2+s2v2+s0v3+s1v3+s2v3) %>% 
  select(-imm1)%>%
  left_join(imm_wave1%>%select(time,sweep_unique,sero_thresh,age_grp, tot, imm1, omega,kappa,foi_sp), 
            by = c("age_grp"="age_grp","time"="time",
                   "sweep_unique"="sweep_unique","sero_thresh"="sero_thresh", 
                   "omega"="omega","kappa"="kappa", "foi_sp"="foi_sp")) %>% 
  mutate(prop_sus = sus_tot/tot,
         prop_imm = imm1/tot)



imm_wave_simp <- imm_wave2%>%
  select(time, sweep_unique, sero_thresh, omega,kappa, age_grp, prop_sus, prop_imm,foi_sp)%>%
  pivot_wider(names_from = "age_grp", values_from = c("prop_sus","prop_imm"))%>%
  unchop(everything())%>%
  left_join(inc_time)
prop_sus_long <- imm_wave_simp%>%select(wave, sweep_unique, sero_thresh,omega, kappa,foi_sp, prop_sus_c:prop_sus_e)%>%
  pivot_longer(cols=prop_sus_c:prop_sus_e, names_to = "age",values_to="prop_sus")%>%
  unchop(everything())%>%
  mutate(age = substr(age,10,10))

prop_imm_long <- imm_wave_simp%>%select(wave, sweep_unique, sero_thresh,omega,kappa, foi_sp,prop_imm_c:prop_imm_e)%>%
  pivot_longer(cols=prop_imm_c:prop_imm_e, names_to = "age",values_to="prop_imm")%>%
  unchop(everything())%>%
  mutate(age = substr(age,10,10))

#saveRDS(prop_sus_long, "prop_sus_long.RDS")
#saveRDS(prop_imm_long, "prop_imm_long.RDS")

## deaths per wave
inc_wave <- inc%>%
  mutate(omega = paste0(omega2,"-",omega4))%>%
  mutate(
    wave = case_when(time<inc_time$time[1]~NA_real_,
                     time<inc_time$time[2]~1,
                     time<inc_time$time[3]~2,
                     time<inc_time$time[4]~3,
                     time<inc_time$time[5]~4,
                     time<inc_time$time[6]~5,
                     time<inc_time$time[7]~6,
                     time<inc_time$time[8]~7,
                     time<inc_time$time[9]~8,
                     time<inc_time$time[10]~9,
                     TRUE~10)
  )%>%
  group_by(sweep_unique, sero_thresh, wave, omega,kappa, foi_sp)%>%
  dplyr::summarise(inc_t = sum(inc_all),
                   inc_c   = sum(inc_c),
                   inc_a   = sum(inc_a),
                   inc_e   = sum(inc_e))

inc_wave_long <- inc_wave%>%
  select(wave, sweep_unique, sero_thresh,omega,kappa, foi_sp, inc_t:inc_e)%>%
  pivot_longer(cols=inc_t:inc_e, names_to = "age",values_to="cases")%>%
  unchop(everything())%>%
  mutate(age = substr(age,5,5))%>%
  mutate(age = ifelse(age=="t","all",age))




##Combine all the above by wave
df_corr_long <- death_wave_long%>%
  left_join(
    sero_wave_long%>%select(-time, -date), 
    by = c("wave"="wave","sweep_unique"="sweep_unique",
           "sero_thresh"="sero_thresh", "age"="age", "omega"="omega","kappa"="kappa",
           "foi_sp"="foi_sp")
  )%>%
  left_join(
    prop_sus_long,
    by = c("wave"="wave","sweep_unique"="sweep_unique",
           "sero_thresh"="sero_thresh", "age"="age", "omega"="omega","kappa"="kappa",
           "foi_sp"="foi_sp")
  )%>%
  left_join(
    prop_imm_long,
    by = c("wave"="wave","sweep_unique"="sweep_unique",
           "sero_thresh"="sero_thresh", "age"="age", "omega"="omega","kappa"="kappa",
           "foi_sp"="foi_sp")
  )%>%
  left_join(
    inc_wave_long,
    by = c("wave"="wave","sweep_unique"="sweep_unique",
           "sero_thresh"="sero_thresh", "age"="age", "omega"="omega","kappa"="kappa",
           "foi_sp"="foi_sp")
  )%>%
  mutate(Year=wave)

df_corr<- df_corr_long%>% filter(!is.na(wave))%>%
  mutate(Year=as.factor(Year),
         omega =paste0(omega2,"-",omega4))

##Correlations
df_corr <- df_corr%>%
  mutate(age_lab = 
           case_when(age=="a"~"Adult",
                     age=="e"~"Older adult",
                     age=="c"~"Child"))%>%
  mutate(age_lab = factor(age_lab, levels=c("Child","Adult","Older adult")),
         kappa = paste0("1","/",kappa),
         omega_lab = case_when(
           omega2==180~"Fast",
           omega2==270~"Medium",
           omega2==365~"Slow"))

corr_tab<- df_corr%>% filter(age!="all")%>%
  filter(wave>0)%>%
  filter(sero_thresh==0)%>%
  
  group_by(omega_lab, kappa, age_lab,age, foi_sp)%>%
  dplyr::summarise(sero_death = round(cor(sero,tot_death, method = "spearman", use="complete.obs"), digits=3),
                   sero_imm  =  round(cor(sero,prop_imm, method = "spearman", use="complete.obs"), digits=3),
                   sero_sus  =  round(cor(sero,prop_sus, method = "spearman", use="complete.obs"), digits=3),
                   sus_death =   round(cor(prop_sus,tot_death,method = "spearman", use="complete.obs"), digits=3),
                   sero_cases = round(cor(sero,cases, method = "spearman", use="complete.obs"), digits=3))


#Visualizing correlations

p1a <- df_corr%>%filter(wave>0 & sero_thresh==0&age=="e")%>%
  #filter(!kappa %in% c("1/1000","1/2000","1/3000"))%>%
  mutate(Year = as.factor(Year))%>%
  
  ggplot(aes(x=sero, y=tot_death, col = as.factor(Year)))+
  geom_point()+ xlab("Seroprevalence")+ylab("Deaths")+
  scale_color_brewer(palette="RdPu", direction=-1, name="Year")+
  theme_bw()+
  facet_grid(vars(kappa),vars(foi_sp))


p1b <-  df_corr%>%filter(wave>0 & sero_thresh==0&age=="e")%>%
  #filter(!kappa %in% c("1/1000","1/2000","1/3000"))%>%
  ggplot(aes(x=sero,y=prop_imm, col = Year))+
  geom_point()+ xlab("Seroprevalence")+ylab("Prop immune")+
  scale_color_brewer(palette="YlGn", direction=-1, name="Year")+
  theme_bw()+
  facet_grid(vars(kappa),vars(foi_sp))

p1c <-  df_corr%>%filter(wave>0 & sero_thresh==0&age=="e")%>%
  filter(!kappa %in% c("1/1000","1/2000","1/3000"))%>%
  ggplot(aes(x=sero, y=prop_sus, col = Year))+
  geom_point()+ xlab("Seroprevalence")+ylab("Relative susceptibility")+
  scale_color_brewer(palette="BuPu", direction=-1, name="Year")+
  theme_bw()+
  facet_grid(vars(kappa),vars(foi_sp))

p1d <- df_corr%>%filter(wave>0 & sero_thresh==0&age=="e")%>%
  filter(!kappa %in% c("1/1000","1/2000","1/3000"))%>%
  ggplot(aes(x=prop_sus, y=tot_death, col = Year))+
  geom_point()+ 
  scale_color_brewer(palette="BuGn", direction=-1, name="Year")+
  theme_bw()+xlab("Proportion susceptible")+ylab("Total deaths")+
  facet_wrap(~omega_lab)

df_corr%>%filter(wave>0 & sero_thresh==0&age=="e")%>%
  #filter(!kappa %in% c("1/1000","1/2000","1/3000"))%>%
  mutate(Year = as.factor(Year))%>%
  
  ggplot(aes(x=sero, y=cases, col = as.factor(Year)))+
  geom_point()+ xlab("Seroprevalence")+ylab("Deaths")+
  scale_color_brewer(palette="RdPu", direction=-1, name="Year")+
  theme_bw()

###
corr_sero_sus <- corr_tab%>%
  ggplot(aes(x=foi_sp, y=kappa))+
  geom_tile(aes(fill=sero_sus))+
  geom_text(aes(label=sero_sus))+
  facet_wrap(~age_lab)+
  scale_fill_gradient2(low ="#D73027", high = "#273871", mid = "white", midpoint = 0)+
  ggtitle("Correlation between seroprevalence and susceptibility at start of waves")+
  xlab("FOI_sp")+ylab("Rate of Waning antibody")

corr_sero_death <- corr_tab%>%
  ggplot(aes(x=foi_sp, y=kappa))+
  geom_tile(aes(fill=sero_death))+
  geom_text(aes(label=sero_death))+
  facet_wrap(~age_lab)+
  scale_fill_gradient2(low ="#D73027", high = "#273871", mid = "white", midpoint = 0)+
  ggtitle("Correlation between seroprevalence at start of waves and death")+
  xlab("FOI_sp")+ylab("Rate of Waning antibody")


corr_sero_case<- corr_tab%>%
  ggplot(aes(x=foi_sp, y=kappa))+
  geom_tile(aes(fill=sero_cases))+
  geom_text(aes(label=sero_cases))+
  facet_wrap(~age_lab)+
  scale_fill_gradient2(low ="#D73027", high = "#273871", mid = "white", midpoint = 0)+
  ggtitle("Correlation between seroprevalence at start of waves and cases")+
  xlab("FOI_sp")+ylab("Rate of Waning antibody")


##Incidence

med<- inc%>%group_by(date,sero_thresh,foi_sp)%>%
  dplyr::summarise(med = median(inc_all),
                   prob_25 = quantile(inc_all, probs=0.10, na.rm=T),
                   prob_75 = quantile(inc_all, probs=0.90, na.rm=T))

p1 <- med %>% ggplot()+
  geom_ribbon(aes(x=date, ymin=prob_25, ymax=prob_75, fill="Range"))+
  theme_bw()+geom_line(aes(x=date, y=med, colour = "Cases \n per 100"), size=0.6)+  
  
  scale_colour_manual("",values="#EF3B2C")+
  scale_fill_manual("",values="gray78")+
  #ggtitle("10-year epidemic projection")+
  xlab("") + ylab("Cases per 100")+
  facet_wrap(~foi_sp, nrow=1)+
  #facet_wrap(~sero_thresh, nrow=1)+
  theme(plot.title = element_text(size=14),axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.text = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title = element_text(size=12),
        #strip.text = element_blank(),
        legend.text = element_text(size=12),
        legend.position = "right",legend.title = element_blank())+
  theme(plot.margin = unit(c(-0.5,0.2,-0.5,1), "line"))+
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 0.7, linetype = "solid"),
        panel.grid.major = element_line(size = 0.001, linetype = 'solid',colour = "white"),
        panel.grid.minor = element_line(size = 0.001, linetype = 'solid',
                                        colour = "white"))


#png("0_plot/inc_strip.png",width = 12,height = 5,units="in",res=400)
p1
#dev.off()

##Seroprevalence
sero <- sero%>%
        pivot_longer(cols= c(sero_c:sero_e, sero_all), names_to = "sero", values_to = "val")

sero_med<- sero%>%group_by(date,sero_thresh,sero,omega2,kappa,foi_sp)%>%
  mutate(sero_thresh=as.character(sero_thresh))%>%
  dplyr::summarise(med = median(val),
                   prob25 = quantile(val, probs=0.25),
                   prob75 = quantile(val, probs=0.75))
 # left_join(thresh_line %>%select(scen, sero_thresh))

sero_med <- sero_med%>%filter(sero!="sero_all")
#sero_med <- sero_med%>%
              #mutate(
              #      scen = factor(scen, levels = c("No vax", "50% thresh", "60% thresh","70% thresh", "80% thresh", "Annual","Biennial"))
              #    )%>%
 # filter(!kappa %in% c(1000,2000,3000))
sero_range_c <- sero_med %>%filter(sero=="sero_c")
sero_range_a <- sero_med %>%filter(sero=="sero_a")
sero_range_e <- sero_med %>%filter(sero=="sero_e")


p2 <-  ggplot()+
  geom_ribbon(data=sero_range_c,aes(x=date, ymin=prob25, ymax=prob75), fill="gray78",alpha=0.5)+
  geom_ribbon(data=sero_range_a,aes(x=date, ymin=prob25, ymax=prob75), fill="gray78", alpha=0.5)+
  geom_ribbon(data=sero_range_e,aes(x=date, ymin=prob25, ymax=prob75), fill="gray78", alpha=0.5)+
  geom_line(data=sero_med,aes(x=date, y = med, col = sero),size=1)+       
  facet_grid(vars(kappa), vars(foi_sp))+
  ylim(0,100) +theme_bw()+
  scale_color_manual(values = c("#4292C6","#C6DBEF","#08306B"),
                     labels=c("Adult",
                              "Child",
                              "Older \nadults"))+
  xlab("")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(size=11),
        strip.text = element_text(size=13),
        legend.position = "right",legend.title = element_blank(), legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,-0.5,1), "line"))+
  #geom_hline(data=thresh_line,aes(yintercept=thresh_num),linetype="dashed",size=0.6)+
  ylab("Seroprevalence\n by age(%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 0.7, linetype = "solid"),
        panel.grid.major = element_line(size = 0.00001, linetype = 'solid',colour = "white"),
        panel.grid.minor = element_line(size = 0.00001, linetype = 'solid',
                                        colour = "white"))

#png("0_sero_novax.png", width = 7,height = 6,units="in",res=500)
p2
#dev.off()
