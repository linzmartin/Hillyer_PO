#PO Analysis 02_10_21
####################################
# clear workspace
rm(list = ls())
#set wd to your project folder
setwd("C:/Users/linzm/Documents/Hillyer Lab/Hillyer_PO")
###########################

#####################
library(readxl)
library(ggplot2)
library(dplyr)
#####################

PO_Data <- read_xlsx("PO_Assay_Data_all.xlsx") #import

################################

PO_Data %>% group_by(Temperature)%>%
  ggplot(aes(x=Time,y=OD,group=Treatment,colour=Treatment,shape=Temperature))+ 
  geom_point()+
  labs(title="PO Assay",
       x="Time (minutes)",
       caption="sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))



######
PO_Data %>%
  ggplot(aes(x=Time,y=OD),group_by(Treatment))+geom_point()+
  geom_smooth(se=FALSE)+facet_wrap(~Temperature+Treatment)+
  labs(title="PO Assay",
       x="Time (minutes)",
       caption="all ages combined")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data %>%
  count(Time,Treatment,Temperature,OD)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 4 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data %>%
  count(Time,Treatment,Temperature,OD,Adult_age_in_days)%>%
  ggplot(aes(x=Time,y=OD,color=Treatment,group=Treatment,shape=Temperature))+ 
  geom_point()+facet_wrap(~Adult_age_in_days)+
  labs(title="PO Assay",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

###########################################

###########
#irrelevant for 1 replicate:
PO_Data <- subset(PO_Data,Treatment!="L-DOPA_H2O")

PO_values_summary <- PO_Data %>%
  group_by(Treatment,Temperature,Time,Adult_age_in_days) %>%
  summarise(mean_OD = mean(OD),
            median_OD = median(OD),
            sd_OD = sd(OD),
            n_OD = n(),
            SE_OD = sd(OD)/sqrt(n()))

PO_values_summary %>%
  ggplot(aes(x=Time,y=mean_OD,shape=Temperature, color=Treatment))+geom_point()+
  labs(title="PO Assay: Mean OD per Treatment",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))

PO_values_summary %>%
  count(Time,Treatment,Temperature,mean_OD,Adult_age_in_days)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment,shape=Temperature))+ 
  geom_point()+facet_wrap(~Adult_age_in_days)+
  labs(title="Mean Melanization Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))

PO_values_summary %>%
  count(Time,Treatment,Temperature,mean_OD,Adult_age_in_days)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment))+ 
  geom_point()+facet_grid(Temperature~Adult_age_in_days)+
  labs(title="Mean Melanization Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))


PO_Data %>%
  count(Time,Treatment,Temperature,OD,Adult_age_in_days)%>%
  ggplot(aes(x=Time,y=OD,color=Treatment,group=Treatment,shape=Temperature))+ 
  geom_point()+facet_wrap(~Adult_age_in_days)+
  labs(title="PO Assay",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490])) #this is not useful for all analysis
################################

PO_Data_1day <- subset(PO_Data,Adult_age_in_days==1)
PO_Data_1day %>%
  count(Time,Treatment,Temperature,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 1 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_3day <- subset(PO_Data, Adult_age_in_days==3)

PO_Data_3day %>%
  count(Time,Treatment,Temperature,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 3 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_5day <- subset(PO_Data,Adult_age_in_days==5)
PO_Data_5day %>%
  count(Time,Treatment,Temperature,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 5 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_7day <- subset(PO_Data,Adult_age_in_days==7)
PO_Data_7day %>%
  count(Time,Treatment,Temperature,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 7 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
################
#Delta OD: 30min-0min
#for each treatment, temperature, age, replicate group
delta_values <- data.frame()

for(i in unique(PO_Data$Treatment)){
  treat_sub <- subset(PO_Data,Treatment==i)
  treat_OD <- data.frame()
  
  for (j in unique(treat_sub$Temperature)){
    Temp_sub <- subset(treat_sub, Temperature==j)
    Temp_OD <- data.frame()
    
    for (k in unique(Temp_sub$Adult_age_in_days)){
      Rep_sub <- subset(Temp_sub, Adult_age_in_days==k)
      Rep_OD <- data.frame()
      
      for (l in unique(Rep_sub$Replicate)){
          OD_0 <- subset(Rep_sub,Rep_sub$Temperature==j & Rep_sub$Time==0 & Rep_sub$Adult_age_in_days==k & Rep_sub$Replicate==l)
          OD_0_val <- OD_0[,5]
          OD_30 <- subset(Rep_sub,Rep_sub$Temperature==j & Rep_sub$Time==30 & Rep_sub$Adult_age_in_days==k & Rep_sub$Replicate==l)
          OD_30_val <- OD_30[,5]
          Delta_OD <- (OD_30_val - OD_0_val)
          names(Delta_OD)[1]<-"Delta_OD"
          treat_OD <-cbind(Treatment=i,Temperature=j,Delta_OD,Age=k,Replicate=l)
          delta_values <- rbind(delta_values,treat_OD)
      }
    }
  }
}

delta_values_summary <- delta_values %>%
  group_by(Treatment,Replicate,Age) %>%
  summarise(Temperature = Temperature, Age = Age,Treatment=Treatment,
            mean_Delta_OD = mean(Delta_OD),
            sd_Delta_OD = sd(Delta_OD),
            n_Delta_OD = n(),
            SE_Delta_OD = sd(Delta_OD)/sqrt(n()))

delta_values<-subset(delta_values,delta_values$Treatment!="L-DOPA_H2O")

delta_values_summary %>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,fill=Treatment))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2,position=position_dodge(0.9))+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Treatment",
       caption="Age=4 days\nMean +/- standard error")+
  ylab(expression("Optical Density"~OD[490]))+ theme_classic()

####
delta_values_summary %>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,fill=Treatment))+
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(~Age)+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2,position=position_dodge(0.9))+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Temperature",
       caption="Error bars represent mean +/- standard error")+
  ylab(expression("Change in OD in 30 Minutes "~OD[490]))+ theme_classic()





#need to make a contour plot w/ delta values / facet grid/wrap
#x: temp, y=delta OD, need to account for age and treatment
delta_values_summary %>%
  count(Replicate,Treatment,Temperature,mean_Delta_OD,Age)%>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,color=Treatment))+
  geom_bar(aes(fill=Treatment),color="black",
           stat="identity",
           position=position_dodge())+
  facet_wrap(~Age)+
  geom_errorbar(aes(ymin=delta_values_summary$mean_Delta_OD - delta_values_summary$SE_Delta_OD,ymax=delta_values_summary$mean_Delta_OD + delta_values_summary$SE_Delta_OD),
                width=0.8,position=position_dodge(0.9))+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Temperature",
       caption="Error bars represent mean +/- standard error")+
  ylab(expression("Change in OD over 30min"~OD[490]))+ theme_bw()


delta_values_summary %>%
  ggplot(aes(x=Temperature,y=Age))+
  geom_tile(aes(fill=mean_Delta_OD))+
  facet_wrap(~Treatment)+
  labs(title="Mean Change in OD Over 30min",
       y="Age",x="Temperature (°C)", fill="Delta OD 490nm")

#####################






################################3
#Friedman one way ANOVA - compare 3+ groups, non-parametric, repeated measures (time series)
#one factor = treatment
library(tidyverse)
library(ggpubr)
library(rstatix)
PO_Data_3 <- subset(PO_Data,Treatment!="L-DOPA_H2O")
PO_Data_3 %>%
  group_by(Treatment,Time)%>%
  get_summary_stats(OD, type="common")

PO_Data_3 %>%
  ggboxplot(x="Time",y="OD",color="Treatment",palette="ico")+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Treatment",
       caption="Age = 4 days, mult temps")+
  ylab(expression("Optical Density"~OD[490]))+ theme_classic()



