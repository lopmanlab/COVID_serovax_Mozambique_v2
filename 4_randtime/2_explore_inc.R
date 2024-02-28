library("dplyr")
library("ggplot2")

mod_scen<- readRDS("0_res/mod_scen_novax.RDS")

tot_pop <- 30066648   ## Took population for 2020 from Mozambique INE (Institute of statistics)
p_urban <- 0.34 ##From INE
p_rural <- 1-p_urban
start.Ns <- c(10884513, 4883969, 7958844, 4900719, 992149, 446454)
dist <- start.Ns/tot_pop

pop_dist = data.frame(age_ur = c("cr","cu","ar","au","er","eu"),
                      pop = start.Ns)

old_pop <-  992149+446454

inc <- list()
n<- length(mod_scen)
for(i in 1:n){
  parms <- mod_scen[[i]]$params 
  
  inc[[i]]<- mod_scen[[i]]$mod_inc[2:1800,]%>%
    select(time, new_I_c, new_I_a, new_I_e, new_I)%>%
    
    mutate(
           #date = seq(from=as.Date("2022-9-1"), to=as.Date("2032-8-28"), by=1),
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

inc<-inc%>%mutate(inc_all = (inc_all/tot_pop)*100)

med<- inc%>%group_by(time,sero_thresh,foi_sp)%>%
  dplyr::summarise(med = median(inc_all),
                   prob_25 = quantile(inc_all, probs=0.10),
                   prob_75 = quantile(inc_all, probs=0.90))

p1 <- med %>% ggplot()+
  geom_ribbon(aes(x=time, ymin=prob_25, ymax=prob_75, fill="Range"))+
  theme_bw()+geom_line(aes(x=time, y=med, colour = "Cases \n per 100"), size=0.6)+  
  
  scale_colour_manual("",values="#EF3B2C")+
  scale_fill_manual("",values="gray78")+
  #ggtitle("10-year epidemic projection")+
  xlab("") + ylab("Cases per 100")+
  facet_wrap(~sero_thresh,nrow=1)


p1