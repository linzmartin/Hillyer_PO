#PO Analysis For Cole's Project - Preliminary Data
#Analysis on 07/01/2021

####################################
# clear workspace
rm(list = ls())
#set wd to your project folder
setwd("C:/Users/linzm/Documents/Hillyer Lab/Hillyer_PO")
###########################

#####################
#load libraries needed:
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
#####################
#read in the data:
PO_Data <- read_xlsx("PO_Assay_Data_Cole_Prelim_07_01_21.xlsx") #import all PO Data
################################
#filter out NA values (unselect controls)
#PO_Data <- subset(PO_Data,Sex=="Male"|Sex=="Female"|Sex=="Both")
PO_Data <- subset (PO_Data,Sample_Type=="Experimental")
#########
PO_graph <- PO_Data %>%
  count(Time,Treatment_Infection, Treatment_PSI,Sex,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=as.factor(Replicate), shape=Treatment_Infection))+ 
  #scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Treatment_PSI~Sex, labeller = label_both)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_graph

PO_graph <- PO_Data %>%
  count(Time,Treatment_Infection, Treatment_PSI,Sex,OD,Replicate)%>%
  ggplot(aes(x=Time,y=OD,color=as.factor(Replicate), shape=Treatment_Infection))+ 
  #scale_shape_identity(guide="legend")+
  geom_point()+
  facet_grid(Sex~Treatment_PSI)+
  labs(title="PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_graph

PO_Graph_Females <- subset(PO_Data,Sex=="Female") %>%
  ggplot(aes(x=Time,y=OD,color=Treatment_PSI,shape=as.factor(Replicate)))+ 
  #scale_shape_identity(guide="legend")+
  geom_point()+
  facet_wrap(~Treatment_PSI, labeller = label_both)+
  labs(title="PO Activity Over Time - Females",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_Females

PO_Graph_Males <- subset(PO_Data,Sex=="Male") %>%
  ggplot(aes(x=Time,y=OD,color=Treatment_PSI,shape=as.factor(Replicate)))+ 
  #scale_shape_identity(guide="legend")+
  geom_point()+
  facet_wrap(~Treatment_PSI, labeller = label_both)+
  labs(title="PO Activity Over Time - Males",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_Males

PO_Graph_Sexes_Combined <- subset(PO_Data,Sex=="Both") %>%
  ggplot(aes(x=Time,y=OD,color=Injector,shape=as.factor(Replicate)))+ 
  #scale_shape_identity(guide="legend")+
  geom_point()+
  facet_wrap(~Treatment_PSI, labeller = label_both)+
  labs(title="PO Activity Over Time - Males & Females Combined\nWith 1xPBS Injection",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))
PO_Graph_Sexes_Combined


################
PO_values_summary <- PO_Data %>%
  group_by(Treatment_PSI,Treatment_Infection,Sex,Time) %>%
  summarise(mean_OD = mean(OD),
            median_OD = median(OD),
            sd_OD = sd(OD),
            n_OD = n(),
            SE_OD = sd(OD)/sqrt(n()))


PO_values_summary %>%
  count(Time,Treatment_PSI,Treatment_Infection,mean_OD,Sex)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment_PSI,shape=Treatment_Infection))+ 
  geom_point()+facet_wrap(~Sex, labeller = label_both)+
  labs(title="Mean Melanization Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))


#PO_3_meantime <- subset(PO_values_summary, Age==3)

PO_values_summary %>%
  count(Time,Treatment,Temp,mean_OD,Age)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment))+ 
  geom_point()+facet_grid(Temp~Age, labeller = label_both)+
  labs(title="Mean PO Activity Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))

PO_values_summary %>%
  count(Time,Treatment_PSI,Treatment_Infection,mean_OD,Sex)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment_PSI,shape=Treatment_Infection))+ 
  geom_point()+facet_grid(Treatment_PSI~Sex, labeller = label_both)+
  labs(title="Mean Melanization Over Time",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))



################################
##################################################################
#Delta OD: 30min-0min
#for each treatment, temperature, age, replicate group
delta_values <- data.frame()

for(i in unique(PO_Data$Treatment_PSI)){
  treat_PSI_sub <- subset(PO_Data,Treatment_PSI==i)
  treat_OD <- data.frame()
  
  for (j in unique(treat_PSI_sub$Treatment_Infection)){
    treat_infection_sub <- subset(treat_PSI_sub, Treatment_Infection==j)
    infect_OD <- data.frame()
    
    sex_OD <- data.frame()
    for (k in unique(treat_infection_sub$Sex)){
      sex_sub <- subset(treat_infection_sub, Sex==k)

        OD_0 <- subset(sex_sub,sex_sub$Treatment_Infection==j & sex_sub$Time==0 & sex_sub$Sex==k)
        OD_0_val <- OD_0[,9]
        OD_30 <- subset(sex_sub,sex_sub$Treatment_Infection==j & sex_sub$Time==30 & sex_sub$Sex==k)
        OD_30_val <- OD_30[,9]
        Delta_OD <- (OD_30_val - OD_0_val)
        names(Delta_OD)[1]<-"Delta_OD"
        treat_OD <-cbind(Treatment_PSI=i,Treatment_Infection=j,Delta_OD,Sex=k)
        delta_values <- rbind(delta_values,treat_OD)
      
    }
  }
}


delta_values_summary <- delta_values %>%
  group_by(Treatment_PSI,Treatment_Infection, Sex) %>%
  summarise(Treatment_PSI = Treatment_PSI,
            Treatment_Infection = Treatment_Infection,
            Sex = Sex,
            mean_Delta_OD = mean(Delta_OD),
            sd_Delta_OD = sd(Delta_OD),
            n_Delta_OD = n(),
            SE_Delta_OD = sd(Delta_OD)/sqrt(n()))

#delta_values<-subset(delta_values,delta_values$Treatment!="L-DOPA_H2O")

####
#this graph accounts for diff ages, temps, and treatments (use this):
delta_values_summary %>%
  ggplot(aes(x=Treatment_PSI,y=mean_Delta_OD,fill=Treatment_Infection))+
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(~Sex, labeller = label_both)+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2,position=position_dodge(0.9))+
  labs(title="Mean PO Activity: Change in Optical Density Over Time",
       x="PSI Treatment",
       caption="Error bars represent mean +/- standard error")+
  ylab(expression("Change in OD in 30 Minutes "~OD[490]))+ theme_classic()

print(delta_values_summary)

deltaboxplot <- ggboxplot(
  delta_values_summary, x="Treatment_PSI",y="mean_Delta_OD",
  color="Treatment_Infection",palette="jco",facet.by = "Sex",nrow=1
)
deltaboxplot




#contour plot:
delta_values_summary %>%
  ggplot(aes(x=Treatment_PSI,y=Treatment_Infection))+
  geom_tile(aes(fill=mean_Delta_OD))+
  facet_wrap(~Sex)+
  labs(title="Mean Change in OD Over 30min",
       y="Treat",x="Temperature (°C)", fill="Delta OD 490nm")
#################################################################

#3 indep variables == factorial / 3 way ANOVA:

#first, convert categories to factors/levels
#levels(delta_values$Treatment_Infection)
as.factor(delta_values$Treatment_Infection)
as.factor(delta_values$Treatment_PSI)
as.factor(delta_values$Sex)


group_by(delta_values,Treatment_PSI,Treatment_Infection,Sex) %>%
  summarise(
    count = n(),
    mean = mean(Delta_OD, na.rm = TRUE),
    sd = sd(Delta_OD, na.rm = TRUE),
    median = median(Delta_OD, na.rm = TRUE),
    IQR = IQR(Delta_OD, na.rm = TRUE)
  )
 #OR:
delta_values %>%
  group_by(Treatment_PSI, Treatment_Infection, Sex)%>%
  get_summary_stats(Delta_OD,type="mean_sd")


bxp <- ggboxplot(
  delta_values, x = "Treatment_PSI", y="Delta_OD",color="Treatment_Infection",
  palette="jco",facet.by="Sex"
)
bxp


#check for outliers:
require(rstatix)
delta_values %>%
  group_by(Treatment_PSI, Treatment_Infection, Sex) %>%
  identify_outliers(Delta_OD)

#check normality / check the residuals:

model  <- lm(Delta_OD ~ Treatment_PSI*Treatment_Infection*Sex, data = delta_values)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

#check for normality by group:
delta_values %>%
  group_by(Treatment_PSI, Treatment_Infection, Sex) %>%
  shapiro_test(Delta_OD)
#sample size is too low^

ggqqplot(delta_values, "Delta_OD", ggtheme = theme_bw()) +
  facet_grid(Treatment_PSI + Sex~Treatment_Infection, labeller = "label_both")
#sample size too low^

#check homogeneity of variance assumption:
delta_values %>% levene_test(Delta_OD ~ Treatment_PSI*Treatment_Infection*Sex)
#too small sample^

#generate anova table:
PO_anova <- delta_values %>% anova_test(Delta_OD ~ Treatment_PSI*Treatment_Infection*Sex)
PO_anova
