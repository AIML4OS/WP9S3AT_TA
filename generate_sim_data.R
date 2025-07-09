# we generate a synthetic population made up of 3 municipalities to showcase our approach

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set wd to source file loacation
library(dplyr)
library(data.table)
library(caret)
library(xgboost)
on.exit(mountSTAT::unmountSessionShares())


jahr <- 2024 #year


pred_vars <- c("GESCHL", #gender
               "GEBSTAAT_OE", #born in Austria (yes/no)
               "STAATB_OE", #austrian citizenship (yes/no)
               "EDU_HAB_NAT", #education
               "age",
               "GEBSTAAT_EU", #born in EU
               "Beruf_cat1", #person works in NACE08 = I55
               "Beruf_cat2", #person works in 
               "Beruf_cat3", #person works in 
               "Beruf_cat0", #works in NACE not listed above
               "Nachtig_pEinw", #overnight stays per capita
               "Betriebe_pEinw", #tourism related businesses per inhabitant
               "Betten_pEinw", #beds per inhabitant
               "einkommen", #income
               "einkommen_q5_anteil", #proportion of people earning more than 4th quantile
               "einkommen_q1_anteil", #proportion of people earning less than 1st quantile
               "ASBESCH_pEinw1", #Total number of employees in workplaces with NACE08 = I55 per municipality (pM)
               "ASUMS_pEinw1", #Sum of revenue of workplaces with NACE08 = I55 (pM)
               "ASBESCH_pEinw2", #Total number of employees in workplaces with NACE08 = I56 (pM)
               "ASUMS_pEinw2", #Sum of revenue of workplaces with NACE08 = I56 (pM)
               "ASBESCH_pEinw3", #Total number of employees in workplaces with NACE08 = R, H49391, H50100, H50300, N77210 per municipality (pM)
               "ASUMS_pEinw3", #Sum of revenue of workplaces with NACE08 = R, H49391, H50100, H50300, N77210per municipality 
               "Ankunft_pEinw_q1", #arrivals per inhabitant in 1st quater
               "Ankunft_pEinw_q2", #arrivals per inhabitant in 2nd quater
               "Ankunft_pEinw_q3", #arrivals per inhabitant in 3rd quater
               "Ankunft_pEinw_q4"  #arrivals per inhabitant in 4th quater
)

# municipalities (three with 100, 1.500 and 30.000 inhabitants respectively)
sim_data <- data.table("areaID"=c(rep("001",100),rep("002",1500),rep("003",30000)))
n <- nrow(sim_data)

# GESCHL
sim_data[,GESCHL:=sample(1:2,n,replace = T, prob = c(0.52,0.48))]

# GEBSTAAT_OE
sim_data[,GEBSTAAT_OE:=sample(c(1,0),n,replace = T, prob = c(0.76,0.24))]

# GEBSTAAT_EU
sim_data[,GEBSTAAT_EU:=sample(c(1,0),n,replace = T, prob = c(0.87,0.13))]

# age (min 15 and max 85 years in survey)
sim_data[,age:=floor(runif(n,15,85))]

# STAATB_OE
sim_data[,STAATB_OE:=sample(c(1,0),n,replace = T, prob = c(0.81,0.19))]

# EDU_HAB_NAT
sim_data[,EDU_HAB_NAT:=sample(0:3,n, replace=T, prob = c(0.03,0.22,0.43,0.32))]

# Beruf_cat1
sim_data[,Beruf_cat1:=sample(0:1,n, replace=T,prob= c(0.98,0.02))]

# Beruf_cat2
sim_data[,Beruf_cat2:=sample(0:1,n,replace=T, prob= c(0.99,0.01))]

# Beruf_cat3
sim_data[,Beruf_cat3:=sample(0:1,n, replace=T,prob = c(0.97,0.03))]

# Beruf_cat0
sim_data[,Beruf_cat0:=sample(0:1,n, replace=T,prob= c(0.95,0.05))]

# Nachtig_pEinw
sim_data[,Nachtig_pEinw:=c(rep(3,100),rep(40,1500),rep(25,30000))]

# Betriebe_pEinw
sim_data[,Betriebe_pEinw:=c(rep(0.002,100),rep(0.2,1500),rep(0.01,30000))]

# Betten_pEinw
sim_data[,Betten_pEinw:=c(rep(0.007,100),rep(0.02,1500),rep(0.15,30000))]

# einkommen
sim_data[,einkommen:=floor(rnorm(n, 37000, 30000))]

# einkommen_q1_anteil
sim_data[,einkommen_q1_anteil:=c(rep(0.17,100),rep(0.2,1500),rep(0.09,30000))]

# einkommen_q5_anteil
sim_data[,einkommen_q5_anteil:=c(rep(0.16,100),rep(0.17,1500),rep(0.10,30000))]

# ASBESCH_pEinw1
sim_data[,ASBESCH_pEinw1:=c(rep(0.008,100),rep(0.03,1500),rep(0.01,30000))]

# ASBESCH_pEinw2
sim_data[,ASBESCH_pEinw2:=c(rep(0.015,100),rep(0.03,1500),rep(0.03,30000))]

# ASBESCH_pEinw3
sim_data[,ASBESCH_pEinw3:=c(rep(0.009,100),rep(0.02,1500),rep(0.01,30000))]

# ASUMS_pEinw1
sim_data[,ASUMS_pEinw1:=c(rep(0.12,100),rep(0.2,1500),rep(0.9,30000))]

# ASUMS_pEinw2
sim_data[,ASUMS_pEinw2:=c(rep(0.6,100),rep(1.5,1500),rep(2,30000))]

# ASUMS_pEinw3
sim_data[,ASUMS_pEinw3:=c(rep(0.2,100),rep(0.6,1500),rep(0.7,30000))]

# Ankunft_pEinw_q1
sim_data[,Ankunft_pEinw_q1:=c(rep(0.2,100),rep(0.6,1500),rep(2,30000))]

# Ankunft_pEinw_q2
sim_data[,Ankunft_pEinw_q2:=c(rep(0.4,100),rep(0.8,1500),rep(5,30000))]

# Ankunft_pEinw_q3
sim_data[,Ankunft_pEinw_q3:=c(rep(0.8,100),rep(1.8,1500),rep(7,30000))]

# Ankunft_pEinw_q4
sim_data[,Ankunft_pEinw_q4:=c(rep(0.2,100),rep(0.3,1500),rep(3,30000))]

# G3_enc -> target variable
sim_data[,G3_enc:=NA]
sim_data[,G3_teilgenommen:=0] #if a person participated in the survey
sim_data[sample(1:n,round(n*0.003)),G3_teilgenommen:=1]

sim_data[G3_teilgenommen==1,G3_enc:=0]
sim_data[sample(which(G3_teilgenommen==1),sum(G3_teilgenommen,na.rm = T)*0.2),G3_enc:=1]

# weights

# age to age classes
sim_data[,age_class:=tourSTAT::altersgruppe(age)]

sim_data[,G3_enc:=as.numeric(G3_enc)]

# population strata
pop_dist <- sim_data[, .N, by = .(age_class, GESCHL)]
# sample strata
sample_dist <- sim_data[G3_teilgenommen == 1, .N, by = .(age_class, GESCHL)]

weights_dt <- merge(pop_dist, sample_dist, by = c("age_class", "GESCHL"), all.x = TRUE, suffixes = c("_pop", "_sample"))

weights_dt[is.na(N_sample), N_sample := 0]

# weights= N/n (per strata)
weights_dt[, weight := fifelse(N_sample > 0, N_pop / N_sample, NA_real_)]

participant_weights <- weights_dt[, .(age_class, GESCHL, weight)]

# Merge weights to original data
sim_data <- merge(sim_data, participant_weights, by = c("age_class", "GESCHL"), all.x = TRUE)

#set weights of non-participants to NA
sim_data[is.na(weight) | G3_teilgenommen == 0, weight := NA]


sim_data[,G3_teilgenommen:=NULL]

sim_data[,ID:=1:nrow(sim_data)]
sim_data[,strata:=paste0(age_class,GESCHL)]

fwrite(sim_data,"sim_data.csv")
