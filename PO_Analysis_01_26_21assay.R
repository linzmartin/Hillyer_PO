#PO Analysis 01_26_21

#####################
library(readxl)
library(ggplot2)
library(dplyr)
#####################

PO_Data <- read_xlsx("PO_Assay_Data_01_26_21.xlsx") #import

################################

PO_Data %>% 
  ggplot(aes(x=Time,y=OD,group=Treatment,colour=Treatment))+ geom_point()+
  labs(title="PO Assay",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))

PO_values_summary <- PO_Data %>%
  group_by(Treatment,Time) %>%
  summarise(mean_OD = mean(OD),
            sd_OD = sd(OD),
            n_OD = n(),
            SE_OD = sd(OD)/sqrt(n()))
PO_values_summary %>%
  ggplot(aes(x=Time,y=mean_OD,group=Treatment,colour=Treatment))+geom_point()+
  labs(title="PO Assay: Means per Treatment",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))

################
#Delta OD: 30min-0min

delta_values <- data.frame()

for(i in unique(PO_Data$Treatment)){
  treat_sub <- subset(PO_Data,Treatment==i)
  treat_OD <- data.frame()
 
  for (j in unique(treat_sub$Replicate)){
  OD_0 <- subset(treat_sub,treat_sub$Replicate==j & treat_sub$Time==0)
  OD_0_val <- OD_0[,4]
  OD_30 <- subset(treat_sub,treat_sub$Replicate==j & treat_sub$Time==30)
  OD_30_val <- OD_30[,4]
  Delta_OD <- (OD_30_val - OD_0_val)
  names(Delta_OD)[1]<-"Delta_OD"
  treat_OD <-cbind(Treatment=i,Replicate=j,Delta_OD)
  delta_values <- rbind(delta_values,treat_OD)
  }
}

delta_values_summary <- delta_values %>%
  group_by(Treatment) %>%
  summarise(mean_Delta_OD = mean(Delta_OD),
            sd_Delta_OD = sd(Delta_OD),
            n_Delta_OD = n(),
            SE_Delta_OD = sd(Delta_OD)/sqrt(n()))

delta_values %>%
  group_by(Treatment)%>%
  #summarise(Delta_OD = mean(Delta_OD)) %>%
  ggplot(aes(x=Treatment,y=Delta_OD))+ geom_boxplot()+
  labs(title="PO Assay: Delta Values per Treatment",
       x="Time (minutes)",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90")+
  ylab(expression("Optical Density"~OD[490]))

delta_values_summary %>%
  ggplot(aes(x=Treatment,y=mean_Delta_OD))+
  geom_col()+
  geom_errorbar(aes(ymin=mean_Delta_OD - SE_Delta_OD,ymax=mean_Delta_OD + SE_Delta_OD),
                width=0.2)+
  labs(title="PO Assay: Mean Delta Values per Treatment",
       x="Treatment",
       caption="n=3 per treatment, sample=20-25 mosquitoes\nHemolymph dilution: 1/50 * 10/90\nMean +/- standard error")+
  ylab(expression("Optical Density"~OD[490]))+ theme_classic()
