install.packages("datarium")

library(tidyverse)
library(ggpubr)
library(rstatix)
set.seed(123)
data("headache", package = "datarium")
headache
headache %>% sample_n_by(gender, risk, treatment, size = 1)

headache %>%
  group_by(gender, risk, treatment) %>%
  get_summary_stats(pain_score, type = "mean_sd")

#visualize
bxp <- ggboxplot(
  headache, x = "treatment", y = "pain_score", 
  color = "risk", palette = "jco", facet.by = "gender"
)
bxp

#check for outliers by groups:
headache %>%
  group_by(gender, risk, treatment) %>%
  identify_outliers(pain_score)

#check normality assumption via model residuals:
model  <- lm(pain_score ~ gender*risk*treatment, data = headache)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

#check normality assumption by groups:
headache %>%
  group_by(gender, risk, treatment) %>%
  shapiro_test(pain_score)

#create qq plot for each cell of design:
ggqqplot(headache, "pain_score", ggtheme = theme_bw()) +
  facet_grid(gender + risk ~ treatment, labeller = "label_both")

#Levene's test for homogeneity of variance assumption:
headache %>% levene_test(pain_score ~ gender*risk*treatment)


#then do 3 way anova
res.aov <- headache %>% anova_test(pain_score ~ gender*risk*treatment)
res.aov
