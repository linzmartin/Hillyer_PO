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
library(ggpubr)
#####################

PO_Data <- read_xlsx("PO_Assay_Data_all.xlsx") #import all PO Data

################################



PO_Data <- PO_Data %>%
  rename(Age = Adult_age_in_days, Temp = Temperature)
PO_Data <- subset(PO_Data,Treatment!="L-DOPA_H2O" & Age!=4)
#########
PO_graph <- PO_Data %>%
  count(Time,Treatment,Temp,OD,Age,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Treatment,shape=Replicate))+ 
  scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Temp~Age, labeller = label_both)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_graph

PO_Graph_27 <- subset(PO_Data,Temp==27) %>%
  ggplot(aes(x=Time,y=OD,color=Treatment,shape=Replicate))+ 
  scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Temp~Age, labeller = label_both)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_27

PO_Graph_30 <- subset(PO_Data,Temp==30) %>%
  ggplot(aes(x=Time,y=OD,color=Treatment,shape=Replicate))+ 
  scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Temp~Age, labeller = label_both)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_30

PO_Graph_32 <- subset(PO_Data,Temp==32) %>%
  ggplot(aes(x=Time,y=OD,color=Treatment,shape=Replicate))+ 
  scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Temp~Age, labeller = label_both)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_32

########
PO_values_summary <- PO_Data %>%
  group_by(Treatment,Temp,Time,Age) %>%
  summarise(mean_OD = mean(OD),
            median_OD = median(OD),
            sd_OD = sd(OD),
            n_OD = n(),
            SE_OD = sd(OD)/sqrt(n()))


PO_values_summary %>%
  count(Time,Treatment,Temp,mean_OD,Age)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment,shape=Temp))+ 
  geom_point()+facet_wrap(~Age, labeller = label_both)+
  labs(title="Mean Melanization Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))


PO_3_meantime <- subset(PO_values_summary, Age==3)

PO_values_summary %>%
  count(Time,Treatment,Temp,mean_OD,Age)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment))+ 
  geom_point()+facet_grid(Temp~Age, labeller = label_both)+
  labs(title="Mean PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))


PO_3_meantime %>%
  count(Time,Treatment,Temp,mean_OD,Age)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment))+ 
  geom_point()+facet_wrap(~Temp, labeller = label_both)+
  labs(title="Mean PO Activity Over Time for 3-Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))
################################

PO_Data_1day <- subset(PO_Data,Age==1)
PO_Data_1day %>%
  count(Time,Treatment,Temp,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temp,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_grid(Temp~Treatment)+
  labs(title="PO Assay: 1 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_3day <- subset(PO_Data, Age==3)

PO_Data_3day %>%
  count(Time,Treatment,Temp,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temp,group=Treatment,shape=factor(Replicate)))+
  geom_point()+facet_grid(Temp~Treatment)+
  labs(title="PO Assay: 3 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_5day <- subset(PO_Data,Age==5)
PO_Data_5day %>%
  count(Time,Treatment,Temp,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temp,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 5 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data_7day <- subset(PO_Data,Age==7)
PO_Data_7day %>%
  count(Time,Treatment,Temp,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temp,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 7 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))


PO_Data_10day <- subset(PO_Data,Age==10)
PO_Data_10day %>%
  count(Time,Treatment,Temp,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=Temp,group=Treatment,shape=factor(Replicate)))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 10 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
##################################################################
#Delta OD: 30min-0min
#for each treatment, temperature, age, replicate group
delta_values <- data.frame()

for(i in unique(PO_Data$Treatment)){
  treat_sub <- subset(PO_Data,Treatment==i)
  treat_OD <- data.frame()
  
  for (j in unique(treat_sub$Temp)){
    Temp_sub <- subset(treat_sub, Temp==j)
    Temp_OD <- data.frame()
    
    for (k in unique(Temp_sub$Age)){
      Rep_sub <- subset(Temp_sub, Age==k)
      Rep_OD <- data.frame()
      
      for (l in unique(Rep_sub$Replicate)){
          OD_0 <- subset(Rep_sub,Rep_sub$Temp==j & Rep_sub$Time==0 & Rep_sub$Age==k & Rep_sub$Replicate==l)
          OD_0_val <- OD_0[,5]
          OD_30 <- subset(Rep_sub,Rep_sub$Temp==j & Rep_sub$Time==30 & Rep_sub$Age==k & Rep_sub$Replicate==l)
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
  group_by(Treatment,Age, Temperature) %>%
  summarise(Temperature = Temperature, Age = Age,Treatment=Treatment,
            Replicate = Replicate,
            mean_Delta_OD = mean(Delta_OD),
            sd_Delta_OD = sd(Delta_OD),
            n_Delta_OD = n(),
            SE_Delta_OD = sd(Delta_OD)/sqrt(n()))

delta_values<-subset(delta_values,delta_values$Treatment!="L-DOPA_H2O")

####
#this graph accounts for diff ages, temps, and treatments (use this):
delta_values_summary %>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,fill=Treatment))+
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(~Age, labeller = label_both)+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2,position=position_dodge(0.9))+
  labs(title="Mean PO Activity: Change in Optical Density Over Time",
       x="Temperature (˚C)",
       caption="Error bars represent mean +/- standard error")+
  ylab(expression("Change in OD in 30 Minutes "~OD[490]))+ theme_classic()


deltaboxplot <- ggboxplot(
  delta_values_summary, x="Temperature",y="mean_Delta_OD",
  color="Treatment",palette="jco",facet.by = "Age",nrow=1
)
deltaboxplot


delta_values_summary %>%
  count(Replicate,Treatment,Temperature,mean_Delta_OD,Age)%>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,color=Treatment))+
  geom_bar(aes(fill=Treatment),color="black",
           stat="identity",
           position=position_dodge())+
  facet_wrap(~Age,labeller = label_both)+
  geom_errorbar(aes(ymin=delta_values_summary$mean_Delta_OD - delta_values_summary$SE_Delta_OD,
                    ymax=delta_values_summary$mean_Delta_OD + delta_values_summary$SE_Delta_OD),
                width=0.8,position=position_dodge(0.9))+
  labs(title="Mean PO Activity: Change in Optical Density Over Time",
       x="Temperature",
       caption="Error bars represent mean +/- standard error")+
  ylab(expression("Change in OD over 30min"~OD[490]))+ theme_bw()

#contour plot:
delta_values_summary %>%
  ggplot(aes(x=Temperature,y=Age))+
  geom_tile(aes(fill=mean_Delta_OD))+
  facet_wrap(~Treatment)+
  labs(title="Mean Change in OD Over 30min",
       y="Age",x="Temperature (°C)", fill="Delta OD 490nm")
#################################################################


