rm(list=ls(all.names=T))
# or rm(list=ls())

#------------------
# Packages
#------------------
library(readxl)
library(ggplot2)
library(gtsummary)
library(gt)
library(webshot2)
library(dplyr)
library(tidyr)


#------------------
# Set directories + load data
#------------------
parent_dir = 'C:/Users/Savan/Documents/Emory/Ma Lab/20'
parent_data_dir = file.path(parent_dir, 'Data')
parent_r_dir = file.path(parent_dir, 'Code')
source(file.path(parent_r_dir, 'GAD7_self_R_functions_SN.R'))


gad7_part1 = read_excel(file.path(parent_data_dir, "GAD7_part1_08282023.xlsx"))
gad7_part2 = read_excel(file.path(parent_data_dir, "GAD7_part2_08282023.xlsx"))


#------------------
# clean
#------------------
# remove patient name
gad7_part1 = gad7_part1[,-2]
gad7_part2 = gad7_part2[,-2]

colnames(gad7_part1) == colnames(gad7_part2)
gad7_combined = rbind.data.frame(gad7_part1, gad7_part2)
rm(gad7_part1, gad7_part2)
colnames(gad7_combined) = c('MRN', 'Gender', 'Age', 'Zip_Code', 'Race', 
                            'Insurance', 'Enrollment_Date', 'GAD_7_value', 
                            'GAD_7_Document_Date', 'PAT_ENC_CSN_ID', 'Department_Name')
nrow(gad7_combined)
table(gad7_combined$Race)
gad7_combined_Race_comb = rep('Others', nrow(gad7_combined))
gad7_combined_Race_comb[grep('Black', gad7_combined$Race)] = 'Black or African American'
gad7_combined_Race_comb[grep('White', gad7_combined$Race)] = 'White or Caucasian'
gad7_combined_Race_comb[grep('Asian', gad7_combined$Race)] = 'Asian'
gad7_combined_Race_comb[grep('Hispanic', gad7_combined$Race)] = 'Hispanic'
gad7_combined_Race_comb[grep('American Indian and Alaskan Native', gad7_combined$Race)] = 'American Indian and Alaskan Native'
gad7_combined$Race_comb = gad7_combined_Race_comb
table(gad7_combined$Race_comb)

# Remove patients less than 18 years old.
gad7_combined = gad7_combined[gad7_combined$Age >= 18,]
nrow(gad7_combined)
# 11521

table(gad7_combined$Department_Name)
# Family Medicine, Primary care, and PSYCH
gad7_combined_depart_name_comb = rep(0, nrow(gad7_combined))
gad7_combined_depart_name_comb[grep('FAMILY', gad7_combined$Department_Name)] = 'FAMILY MED'
gad7_combined_depart_name_comb[grep('FAM', gad7_combined$Department_Name)] = 'FAMILY MED'
gad7_combined_depart_name_comb[grep('PSYCH', gad7_combined$Department_Name)] = 'PSYCH'
gad7_combined_depart_name_comb[grep('PRIMARY', gad7_combined$Department_Name)] = 'PRIMARY CARE'
gad7_combined_depart_name_comb[grep('CAMP CREEK MEDICINE', gad7_combined$Department_Name)] = 'FAMILY MED'
gad7_combined$Department_Category = gad7_combined_depart_name_comb
table(gad7_combined$Department_Category, useNA='ifany')

# remove redundant measurement (especially with insurance type)
gad7_dat_final = aggregate(gad7_combined[,c('MRN', 'Gender', 'Age', 'Race', 'Enrollment_Date', 
                                            'GAD_7_value', 'Department_Name', 
                                            'Race_comb', 'Department_Category')],
                           by=list(GAD_7_Document_Date=gad7_combined$GAD_7_Document_Date, 
                                   PAT_ENC_CSN_ID=gad7_combined$PAT_ENC_CSN_ID), unique)
rm(gad7_combined)
colnames(gad7_dat_final)
sapply(1:11, function(x) class(gad7_dat_final[,x]) == 'list')
# Race exists multiple options.

race_ambiguious_ids = which(sapply(1:nrow(gad7_dat_final), function(x) ifelse(length(gad7_dat_final$Race[[x]]) > 1, x, '')) != '')
gad7_dat_final$Race[race_ambiguious_ids]
gad7_dat_final$Race_comb[race_ambiguious_ids]
gad7_dat_final = gad7_dat_final[-race_ambiguious_ids,]
gad7_dat_final = gad7_dat_final[order(gad7_dat_final$MRN, gad7_dat_final$Enrollment_Date),]
# confirm it has been removed
race_ambiguious_ids = which(sapply(1:nrow(gad7_dat_final), function(x) ifelse(length(gad7_dat_final$Race[[x]]) > 1, x, '')) != '')
nrow(gad7_dat_final) # 4863
gad7_dat_final$Race = unlist(gad7_dat_final$Race)
gad7_dat_final$Race_comb = unlist(gad7_dat_final$Race_comb)

# Patient MRN
MRN_unique_id_vec = unique(gad7_dat_final$MRN)
print(length(MRN_unique_id_vec))  # 2191
sum(table(gad7_dat_final$MRN) == 1)  # 1072 of 2191 have single observation.
plot(density(table(gad7_dat_final$MRN)), main='Repeated Measurement')


# Order the gad7_dat_final by MRN and GAD_7_Document_Date
gad7_dat_final = gad7_dat_final[order(gad7_dat_final$MRN, gad7_dat_final$GAD_7_Document_Date),]


gad7_dat_final_visit_ids_vec = NULL
for (i in 1:length(MRN_unique_id_vec)) {
  MRN_unique_id_i = MRN_unique_id_vec[i]
  gad7_dat_final_visit_ids_vec = c(gad7_dat_final_visit_ids_vec, 0:(sum(gad7_dat_final$MRN==MRN_unique_id_i)-1))
}
gad7_dat_final$Visit_Num = gad7_dat_final_visit_ids_vec


gad7_dat_final_visit_time_relative_vec = NULL
for (i in 1:length(MRN_unique_id_vec)) {
  MRN_unique_id_i = MRN_unique_id_vec[i]
  MRN_unique_id_i_size = sum(gad7_dat_final$MRN == MRN_unique_id_i)
  if (sum(gad7_dat_final$MRN == MRN_unique_id_i) == 1) {
    gad7_dat_final_visit_time_relative_vec = c(gad7_dat_final_visit_time_relative_vec, 0)
  } else {
    # the date may not be ordered.
    visit_time_relative_i = sort(gad7_dat_final$GAD_7_Document_Date[gad7_dat_final$MRN == MRN_unique_id_i])
    visit_time_relative_i = cumsum(c(0, difftime(visit_time_relative_i[2:MRN_unique_id_i_size], visit_time_relative_i[1:(MRN_unique_id_i_size-1)], units='days')))
    gad7_dat_final_visit_time_relative_vec = c(gad7_dat_final_visit_time_relative_vec, visit_time_relative_i)
    if (sum((visit_time_relative_i < 0)) > 0) {
      print(i); print(MRN_unique_id_i)
    }
  }
}
gad7_dat_final$Visit_Time_Relative = gad7_dat_final_visit_time_relative_vec
summary(gad7_dat_final_visit_time_relative_vec)  # Now the minimum value is 0, which makes sense.


#------------------
# Exclude individuals who do not meet inclusion criteria 
#------------------
# Remove single-participant observation
gad7_dat_final_2 = gad7_dat_final[gad7_dat_final$MRN %in% unique(gad7_dat_final$MRN)[table(gad7_dat_final$MRN)>1],]
gad7_dat_final_2_dir = file.path(parent_data_dir, 'GAD-7-combine-no-single-obs-2023-08-30.RDS')
saveRDS(gad7_dat_final_2, gad7_dat_final_2_dir)
gad7_dat_final_2_dir = file.path(parent_data_dir, 'GAD-7-combine-no-single-obs-2023-08-30.csv')
write.csv(gad7_dat_final_2, gad7_dat_final_2_dir)

# Confirm whether each patient has a PSYCH visit result
print(length(unique(gad7_dat_final_2$MRN)))
print(length(unique(gad7_dat_final_2$MRN[gad7_dat_final_2$Department_Category == 'PSYCH'])))
# 1030 out of 1119 patients receive a PSYCH visit among those who have more than one visit.
setdiff(unique(gad7_dat_final_2$MRN), unique(gad7_dat_final_2$MRN[gad7_dat_final_2$Department_Category == 'PSYCH']))

print(length(unique(gad7_dat_final_2$MRN)))


#------------------
# Clinicians created a list of additional eligible patients, add them to the df 
#------------------
# Create a df with the additional eligible patients
additional_eligible <- read_excel(file.path(parent_data_dir, "gad7_Completedvisits.xlsx"))

mrn_to_delete <- setdiff(unique(gad7_dat_final_2$MRN), unique(gad7_dat_final_2$MRN[gad7_dat_final_2$Department_Category == 'PSYCH']))
mrn_to_delete_2 <- setdiff(mrn_to_delete, unique(additional_eligible$MRN))

gad7_dat_final_3 <- gad7_dat_final_2[!(gad7_dat_final_2$MRN %in% mrn_to_delete_2), ]
print(length(unique(gad7_dat_final_3$MRN)))

summary(gad7_dat_final_3$Visit_Time_Relative)  # the minimum value is still 0, order is preserved.

gad7_psyc_only <- subset(gad7_dat_final_3, gad7_dat_final_3$Department_Category == 'PSYCH')
# View(gad7_psyc_only)

# Rerunning plots with final dataset
plot(density(table(gad7_psyc_only$MRN)), main='Repeated Measurement')



#------------------
# spaghetti plots of gad7 
#------------------
# Visit_Num
# Total
parent_visit_id_dir = file.path(parent_dir, 'Results')
p_spaghetti_gad7_overall_visit_id = compute_mean_trend_wrt_visit_num(gad7_psyc_only, NULL)
p_spaghetti_gad7_overall_visit_id_dir = file.path(parent_visit_id_dir, 'p_spaghetti_gad7_PSYCH_only_overall.png')
ggsave(p_spaghetti_gad7_overall_visit_id_dir, p_spaghetti_gad7_overall_visit_id, units='mm', width=400, height=200, dpi=300)

# gender
p_spaghetti_gad7_visit_id_gender = compute_mean_trend_wrt_visit_num(gad7_psyc_only, 'Gender')
p_spaghetti_gad7_visit_id_gender_dir = file.path(parent_visit_id_dir, 'p_spaghetti_gad7_PSYCH_only_id_gender.png')
ggsave(p_spaghetti_gad7_visit_id_gender_dir, p_spaghetti_gad7_visit_id_gender, units='mm', width=400, height=200, dpi=300)

# Race
p_spaghetti_gad7_visit_id_race = compute_mean_trend_wrt_visit_num(gad7_dat_final_3, 'Race')
p_spaghetti_gad7_visit_id_race_dir = file.path(parent_visit_id_dir, 'p_spaghetti_gad7_PSYCH_only_id_race.png')
ggsave(p_spaghetti_gad7_visit_id_race_dir, p_spaghetti_gad7_visit_id_race, units='mm', width=450, height=200, dpi=300)

gad7_dat_final_3$Race2 = gad7_dat_final_3$Race_comb
gad7_dat_final_3$Race2[gad7_dat_final_3$Race_comb == 'Asian'] = 'Others'
gad7_dat_final_3$Race2[gad7_dat_final_3$Race_comb == 'American Indian and Alaskan Native'] = 'Others'
table(gad7_dat_final_3$Race2)
gad7_dat_final_3$Race2 = factor(gad7_dat_final_3$Race2, levels=c('Black or African American', 'Hispanic', 'White or Caucasian', 'Others'))
p_spaghetti_gad7_visit_id_race_2 = compute_mean_trend_wrt_visit_num(gad7_dat_final_3, 'Race2')
p_spaghetti_gad7_visit_id_race_2_dir = file.path(parent_visit_id_dir, 'p_spaghetti_gad7_PSYCH_only_id_race_collapsed.png')
ggsave(p_spaghetti_gad7_visit_id_race_2_dir, p_spaghetti_gad7_visit_id_race_2, units='mm', width=400, height=200, dpi=300)


#------------------
# Create GAD score #1 and GAD score a year later
#------------------
gad7_psyc_only_subset <- gad7_psyc_only %>%
  group_by(MRN) %>%
  mutate(GAD_Score_1 = GAD_7_value[which.min(Visit_Time_Relative)])

gad7_psyc_only_subset <- gad7_psyc_only_subset %>%
  group_by(MRN) %>%
  mutate(GAD_Score_Year_Later = ifelse(Visit_Time_Relative < 365, 
                                       GAD_7_value[which.min(abs(abs(Visit_Time_Relative) - 365))], 
                                       NA))
head(gad7_psyc_only_subset)
unique_gad7_dat <- gad7_psyc_only_subset %>%
  select(MRN, GAD_Score_Year_Later, GAD_Score_1) %>%
  distinct(MRN, .keep_all = T)

# Adding a column with difference in GAD score
unique_gad7_dat <- unique_gad7_dat %>%
  mutate(Difference = GAD_Score_Year_Later - GAD_Score_1)
unique_gad7_dat <- na.omit(unique_gad7_dat)


#------------------
# Test diff in gad7 score initial and 1-year
#------------------
# normality test 
head(unique_gad7_dat)
hist(unique_gad7_dat$Difference, main = "Histogram, GAD_Score_Year_Later - GAD_Score_1")
plot(density(unique_gad7_dat$Difference), main='Density, GAD_Score_Year_Later - GAD_Score_1')
shapiro.test(unique_gad7_dat$Difference)  # indeed not normal

# overall pattern using Wilcoxon signed rank test
wilcox.test(unique_gad7_dat$GAD_Score_1, unique_gad7_dat$GAD_Score_Year_Later, paired=T)

#------------------
# Table of gad7 at initial, 1 year, and diff between the two 
#------------------
unique_gad7_overall_summary <-
  unique_gad7_dat[,2:4] %>% tbl_summary(
    statistic = list(c('GAD_Score_1', 'GAD_Score_Year_Later', 'Difference') ~ "{median} ({p25}, {p75})"),
    digits=list(c('GAD_Score_1', 'GAD_Score_Year_Later', 'Difference') ~ c(1, 1, 1))
  ) %>%
  modify_caption("**GAD-7 Scores**") %>%
  bold_labels()
unique_gad7_overall_summary_gt = as_gt(unique_gad7_overall_summary)
file_path = file.path(parent_visit_id_dir, "tab_overall_gad7_continuous_summary.png")
gtsave(unique_gad7_overall_summary_gt, file = file_path)


#------------------
# Create categorical GAD7 score
#------------------
unique_gad7_dat$"Initial_GAD7_Score_Discrete" <- 
  ifelse(unique_gad7_dat$GAD_Score_1 >= 0 & unique_gad7_dat$GAD_Score_1 <= 4, "Minimal (0-4)",
         ifelse(unique_gad7_dat$GAD_Score_1 >= 5 & unique_gad7_dat$GAD_Score_1 <= 9, "Mild (5-9)", 
                ifelse(unique_gad7_dat$GAD_Score_1 >= 10 & unique_gad7_dat$GAD_Score_1 <= 14, "Moderate (10-14)", 
                       el("Severe (15-21)"))))
unique_gad7_dat$"Year_Follow_up_GAD7_Score_Discrete" <- 
  ifelse(unique_gad7_dat$GAD_Score_Year_Later >= 0 & unique_gad7_dat$GAD_Score_Year_Later <= 4, "Minimal (0-4)",
         ifelse(unique_gad7_dat$GAD_Score_Year_Later >= 5 & unique_gad7_dat$GAD_Score_Year_Later <= 9, "Mild (5-9)", 
                ifelse(unique_gad7_dat$GAD_Score_Year_Later >= 10 & unique_gad7_dat$GAD_Score_Year_Later <= 14, "Moderate (10-14)", 
                       el("Severe (15-21)"))))
head(unique_gad7_dat)
sum(is.na(unique_gad7_dat))

# Reformat the data (not sure if this is the most efficient way to do this, but I wanted it to match the way the data was formatted in the Friedman Rank Sum Test on the site)
reformatted_data <- unique_gad7_dat %>%
  select(MRN, GAD_Score_1, GAD_Score_Year_Later, Initial_GAD7_Score_Discrete, Year_Follow_up_GAD7_Score_Discrete) %>%
  pivot_longer(
    cols = c(GAD_Score_1, GAD_Score_Year_Later),
    names_to = "Initial_or_Year_Follow_up",
    values_to = "GAD7_Score"
  ) %>%
  mutate(
    Initial_or_Year_Follow_up = ifelse(Initial_or_Year_Follow_up == "GAD_Score_1", 0, 1),
    GAD7_Discrete = ifelse(Initial_or_Year_Follow_up == 0, Initial_GAD7_Score_Discrete, Year_Follow_up_GAD7_Score_Discrete)
  ) %>%
  select(MRN, Initial_or_Year_Follow_up, GAD7_Score, GAD7_Discrete)
head(reformatted_data)



#------------------
# Friedman test 
#------------------
friedman.test(y=reformatted_data$GAD7_Score, groups=reformatted_data$Initial_or_Year_Follow_up, blocks=reformatted_data$MRN)

GAD7_Discrete_summary <- reformatted_data %>%
  ungroup() %>%
  select(GAD7_Discrete, Initial_or_Year_Follow_up)

GAD7_Discrete_summary$GAD7_Discrete = factor(
  GAD7_Discrete_summary$GAD7_Discrete, 
  levels=c("Minimal (0-4)", "Mild (5-9)", "Moderate (10-14)", "Severe (15-21)")
)

#------------------
# Create table to visualize the number of individuals in each gad7 category
#------------------
GAD7_Discrete_Table <-
  GAD7_Discrete_summary %>%
  tbl_summary(
    by = "Initial_or_Year_Follow_up",
    statistic = "GAD7_Discrete" ~ "({n}, {p}%)",
    digits = "GAD7_Discrete" ~ c(0, 1),
  ) %>% 
  modify_caption("**GAD7 Severity Change**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "Initial (0) or Year Follow-up (1)") %>%
  bold_labels()
GAD7_Discrete_Table_gt <- as_gt(GAD7_Discrete_Table)
file_path = file.path(parent_visit_id_dir, "tab_gad7_pairwise_contingency_table.png")
gtsave(GAD7_Discrete_Table_gt, file = file_path)


