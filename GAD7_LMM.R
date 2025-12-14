rm(list=ls(all.names=T))
# or rm(list=ls())
#------------------
# Packages
#------------------
install.packages("Matrix", dependencies = TRUE)
library(readxl)
library(ggplot2)
library(nlme)
library(lme4)
library(lmerTest)
library(tidyverse)


#------------------
# Set directories + load data
#------------------
parent_dir = '/cloud/project'
parent_data_dir = file.path(parent_dir, 'Datasets')
parent_r_dir = file.path(parent_dir, 'Code')
parent_r_output_dir = file.path(parent_dir, 'Results', 'R_Output')
source(file.path(parent_r_dir, 'GAD7_self_R_functions_SN.R'))

gad7_dat_dir = file.path(parent_data_dir, 'gad7_fin.rds')
gad7_dat_fin = readRDS(gad7_dat_dir)



#------------------
# Data exploration + light cleaning
#------------------
print(length(unique(gad7_dat_fin$MRN)))
head(gad7_dat)

# relevel to set reference groups
gad7_dat$Gender = relevel(factor(gad7_dat$Gender), ref='Male')
gad7_dat$Race2 = relevel(factor(gad7_dat$Race2), ref='Black or African American')
gad7_dat$Department_Category = relevel(factor(gad7_dat$Department_Category), ref='PSYCH')
gad7_dat$Visit_Time_Relative = gad7_dat$Visit_Time_Relative / 7 # weeks

# remove visits w/ FAMILY MED and PRIMARY
gad7_psyc_only <- subset(gad7_dat, gad7_dat$Department_Category == 'PSYCH')
View(gad7_psyc_only)

# Create age categories 
gad7_psyc_only$Age_Grouped = cut(gad7_psyc_only$Age, breaks = c(0, 34, 44, 54, 64, Inf),
                                 labels=paste('Age: ', c('(18,34]', '(35,44]', '(45,54]',
                                                         '(55,64]', '(>=65]'), sep=''))
table(gad7_psyc_only$Age_Grouped, useNA='ifany')
# Age: (18,34] Age: (35,44] Age: (45,54] Age: (55,64]  Age: (>=65] 
# 408          538          671          618          367 



#------------------
# paired t-test (no covariate adjustment)
#------------------
gad7_dat_first = aggregate(gad7_psyc_only$GAD_7_value, 
                           by=list(MRN=gad7_psyc_only$MRN), FUN=head, 1)
gad7_dat_last = aggregate(gad7_psyc_only$GAD_7_value, 
                          by=list(MRN=gad7_psyc_only$MRN), FUN=tail, 1)
gad7_dat_change = merge(x=gad7_dat_first, y=gad7_dat_last, by='MRN')
colnames(gad7_dat_change) = c('MRN', 'First', 'Last')
t.test(gad7_dat_change$First, gad7_dat_change$Last, paired=T)
wilcox.test(gad7_dat_change$First, gad7_dat_change$Last, paired=T)

summary(gad7_dat_change$First)
hist(gad7_dat_change$First)
summary(gad7_dat_change$Last)
hist(gad7_dat_change$Last)



#------------------
# Create df w/ change in gad7 score
#------------------
gad7_dat_change_df = data.frame(
  value = c(gad7_dat_change$First, gad7_dat_change$Last),
  visit_time = rep(c('First', 'Last'), each=nrow(gad7_dat_change)),
  MRN=rep(gad7_dat_change$MRN, 2)                 
)
p_gad7_change_df = ggplot(data=gad7_dat_change_df, aes(x=visit_time, y=value, group=MRN)) +
  geom_point() + geom_line(alpha=0.25)
p_gad7_change_df



#------------------
# Run LMMs 
#------------------
gad7_lmm_1 <- lmer(GAD_7_value ~ Age_Grouped + Gender + Race2 + 
                     Visit_Time_Relative + (1|MRN),
                   REML=F, data=gad7_psyc_only, na.action = na.exclude)
summary(gad7_lmm_1)
anova(gad7_lmm_1)
AIC(gad7_lmm_1) # 16058.14

gad7_lmm_1.1 <- lmer(GAD_7_value ~ Age_Grouped + Gender + Race2 + 
                       Visit_Num + (1|MRN),
                     REML=F, data=gad7_psyc_only, na.action = na.exclude)
summary(gad7_lmm_1.1)
anova(gad7_lmm_1.1)
AIC(gad7_lmm_1.1) # 15937.65

gad7_lmm_2 <- lmer(GAD_7_value ~ Age_Grouped + Gender + Race2 + 
                     Visit_Time_Relative + (1+Visit_Num|MRN), REML=F, data=gad7_psyc_only, na.action = na.exclude)
summary(gad7_lmm_2)
anova(gad7_lmm_2)
AIC(gad7_lmm_2) # 16008.24
anova(gad7_lmm_2, gad7_lmm_1)

gad7_lmm_2.1 <- lmer(GAD_7_value ~ Age_Grouped + Gender + Race2 + 
                       Visit_Num + (1+Visit_Num|MRN), REML=F, data=gad7_psyc_only, na.action = na.exclude)
summary(gad7_lmm_2.1)
anova(gad7_lmm_2.1)
AIC(gad7_lmm_2.1) # 15921.38
anova(gad7_lmm_2.1, gad7_lmm_1.1)
str(resid(gad7_lmm_2.1))



#------------------
# Use model 2.1 based on AIC
#------------------
gad7_lmm_2.1_df = lme4::fortify.merMod(gad7_lmm_2.1)
summary(gad7_lmm_2.1)$coefficients[,1] - 1.96 * summary(gad7_lmm_2.1)$coefficients[,2]
data.frame(summary(gad7_lmm_2.1)$coefficients, Low=summary(gad7_lmm_2.1)$coefficients[,1] - 1.96 * summary(gad7_lmm_2.1)$coefficients[,2], 
           Upp=summary(gad7_lmm_2.1)$coefficients[,1] + 1.96 * summary(gad7_lmm_2.1)$coefficients[,2])



#------------------
# Produce LMM plots for age/gender/race using LMM 2.1 
#------------------
class(gad7_lmm_2.1)
p_gad7_lmm_2.1_age_categorical = produce_lmm_fitted_plot(gad7_lmm_2.1_df, 'Age_Grouped')
p_gad7_lmm_2.1_age_categorical_dir = file.path(parent_r_output_dir, 'p_gad7_lmm_age_categorical_visit_time.png')
ggsave(p_gad7_lmm_2.1_age_categorical_dir, p_gad7_lmm_2.1_age_categorical, units='mm', width=400, height=200, dpi=300)

p_gad7_lmm_2.1_gender = produce_lmm_fitted_plot(gad7_lmm_2.1_df, 'Gender')
p_gad7_lmm_2.1_gender_dir = file.path(parent_r_output_dir, 'p_gad7_lmm_gender_visit_time.png')
ggsave(p_gad7_lmm_2.1_gender_dir, p_gad7_lmm_2.1_gender, units='mm', width=300, height=200, dpi=300)

p_gad7_lmm_2.1_race = produce_lmm_fitted_plot(gad7_lmm_2.1_df, 'Race2')
p_gad7_lmm_2.1_race_dir = file.path(parent_r_output_dir, 'p_gad7_lmm_race_visit_time.png')
ggsave(p_gad7_lmm_2.1_race_dir, p_gad7_lmm_2.1_race, units='mm', width=450, height=200, dpi=300)


