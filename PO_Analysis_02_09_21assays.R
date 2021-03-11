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

PO_Data <- read_xlsx("PO_Assay_Data_02_09_21.xlsx", sheet="Age4_Rep1") #import

################################

PO_Data %>% group_by(Temperature)%>%
  ggplot(aes(x=Time,y=OD,group=Treatment,colour=Treatment,shape=Temperature))+ 
  geom_point()+
  labs(title="PO Assay",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))



######
PO_Data %>%
  ggplot(aes(x=Time,y=OD),group_by(Treatment))+geom_point()+
  geom_smooth(se=FALSE)+facet_wrap(~Temperature+Treatment)

PO_Data %>%
  count(Time,Treatment,Temperature,OD)%>%
  ggplot(aes(x=Time,y=OD,color=Temperature,group=Treatment))+ 
  geom_point()+facet_wrap(~Treatment)+
  labs(title="PO Assay: 4 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))

PO_Data %>%
  count(Time,Treatment,Temperature,OD)%>%
  ggplot(aes(x=Time,y=OD,color=Treatment,group=Treatment))+ 
  geom_point()+facet_wrap(~Temperature)+
  labs(title="PO Assay: 4 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Optical Density"~OD[490]))



###########
#irrelevant for 1 replicate:
PO_values_summary <- PO_Data %>%
  group_by(Treatment,Temperature,Time) %>%
  summarise(mean_OD = mean(OD),
            median_OD = median(OD),
            sd_OD = sd(OD),
            n_OD = n(),
            SE_OD = sd(OD)/sqrt(n()))

PO_values_summary %>%
  ggplot(aes(x=Time,y=mean_OD,shape=Temperature, color=Treatment))+geom_point()+
  geom_line()+
  labs(title="PO Assay: Mean OD per Treatment",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))

PO_values_summary %>%
  count(Time,Treatment,Temperature,mean_OD)%>%
  ggplot(aes(x=Time,y=mean_OD,color=Treatment,group=Treatment))+ 
  geom_point()+facet_wrap(~Temperature)+
  labs(title="PO Assay: 4 Day-Old Adults",
       x="Time (minutes)")+
  ylab(expression("Mean Optical Density"~OD[490]))


################
#Delta OD: 30min-0min

delta_values <- data.frame()

for(i in unique(PO_Data$Treatment)){
  treat_sub <- subset(PO_Data,Treatment==i)
  treat_OD <- data.frame()
  
  for (j in unique(treat_sub$Temperature)){
    OD_0 <- subset(treat_sub,treat_sub$Temperature==j & treat_sub$Time==0)
    OD_0_val <- OD_0[,5]
    OD_30 <- subset(treat_sub,treat_sub$Temperature==j & treat_sub$Time==30)
    OD_30_val <- OD_30[,5]
    Delta_OD <- (OD_30_val - OD_0_val)
    names(Delta_OD)[1]<-"Delta_OD"
    Age = OD_0[,6]
    treat_OD <-cbind(Treatment=i,Temperature=j,Delta_OD,Age=Age)
    delta_values <- rbind(delta_values,treat_OD)
  }
}

delta_values_summary <- delta_values %>%
  group_by(Treatment) %>%
  summarise(Temperature = Temperature, Age = Adult_age_in_days,
            mean_Delta_OD = mean(Delta_OD),
            sd_Delta_OD = sd(Delta_OD),
            n_Delta_OD = n(),
            SE_Delta_OD = sd(Delta_OD)/sqrt(n()))

delta_values<-subset(delta_values,delta_values$Treatment!="L-DOPA_H2O")

delta_values %>%
  ggplot(aes(x=Treatment,y=Delta_OD,color=Temperature))+
  geom_point()


delta_values_summary %>%
  ggplot(aes(x=Temperature,y=mean_Delta_OD,fill=Treatment))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2,position=position_dodge(0.9))+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Treatment",
       caption="Age=4 days\nMean +/- standard error")+
  ylab(expression("Optical Density"~OD[490]))+ theme_classic()


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




bxp <- ggboxplot(
  selfesteem2, x = "time", y = "score",
  color = "treatment", palette = "jco"
)
bxp

selfesteem2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)

selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ treatment, labeller = "label_both")

res.aov <- anova_test(
  data = selfesteem2, dv = score, wid = id,
  within = c(treatment, time)
)
get_anova_table(res.aov)
###################
res.fried <- PO_Data_3 %>% friedman_test(OD ~ Time |)
res.fried

PO_Data_3 %>% friedman_effsize()
selfesteem %>% friedman_effsize(score ~ time |id)




