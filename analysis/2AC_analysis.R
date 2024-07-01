###### 2-Alternative Choice Test Analysis ######
# Import Libraries ----
library("dplyr")
library("rstatix")
library("ggplot2")
library("ggpubr")
library("data.table")
library("MASS")
library("tidyr")

# Initialization ----
project_folder = "H:\\My Drive\\Katz-Lab_Otter-Data\\Projects\\Social_Feeding"
setwd(project_folder)
experimental_data = read.csv(".\\datasets\\2-alternative-choice\\cleaned_data\\2AC_choice_data_20231114.csv")
save_analysis_as_csv = TRUE
analysis_path = "H:\\My Drive\\Katz-Lab_Otter-Data\\Projects\\Social_Feeding\\analysis\\"

set.seed(68837)

# 7-days Food Deprived ----
## Data Summary ----
experimental_data_sum_7 <- experimental_data %>%
  filter(Food_Deprivation_Length == 7) %>%
  group_by(Choice_1, Acclimation) %>%
  summarise(total_trial = n(),
            num_sucesses = sum(Choice))%>%
  mutate(p_value = NA, CI_95_upper = NA, CI_95_lower = NA,
         prob_sucess = num_sucesses/total_trial)

## Binomial Test ----
for(i in 1:nrow(experimental_data_sum_7)) {
  analysis_result <- binom.test(x = experimental_data_sum_7$num_sucesses[i],
                                n = experimental_data_sum_7$total_trial[i],
                                p = 0.5,
                                alternative = "two.sided")
  
  experimental_data_sum_7$p_value[i] = as.numeric(analysis_result[3])
  experimental_data_sum_7$CI_95_lower[i] = as.numeric(unlist(analysis_result[4])[1])
  experimental_data_sum_7$CI_95_upper[i] = as.numeric(unlist(analysis_result[4])[2])
  
  assign("experimental_data_sum_7", experimental_data_sum_7, envir = .GlobalEnv)
}

## Anemone Size ----
size_diff_7 <- experimental_data %>%
  dplyr::select(Experiment_ID, Choice, Choice_1, Food_Deprivation_Length, Acclimation, radius_chosen, radius_other) %>%
  filter(Food_Deprivation_Length == 7) %>%
  mutate(radius_diff = radius_chosen-radius_other)

hist(size_diff_7$radius_diff, breaks = 10)

size_diff_7 %>% identify_outliers(radius_diff)
size_diff_7 %>% shapiro_test(radius_diff) 
size_diff_7 %>% t_test(radius_diff ~ 1, mu = 0)
size_diff_7 %>% cohens_d(radius_diff ~ 1, mu = 0)

#checking with an anova
size_diff_7$Choice_1 <- as.factor(size_diff_7$Choice_1)
levels(size_diff_7$Choice_1)

ggplot(size_diff_7, aes(x = Choice_1, y = radius_diff))+
  geom_boxplot() +
  geom_jitter()

size.aov_7 <- aov(radius_diff ~ Choice_1, data = size_diff_7)
size_aov_tukey_7 <- TukeyHSD(size.aov_7)

#homogeneity of variances
plot(size.aov, 1)
car::leveneTest(radius_diff ~ Choice_1, data = size_diff_7) #variance across groups is statistically significant
oneway.test(radius_diff ~ Choice_1, data = size_diff_7)
pairwise.t.test(size_diff_7$radius_diff, size_diff_7$Choice_1,
                p.adjust.method = "BH", pool.sd = FALSE)
#normality
plot(size.aov, 2)
aov_residuals <- residuals(object = size.aov)
shapiro.test(x = aov_residuals)

size_kruskal_7 <- kruskal.test(radius_diff ~ Choice_1, data = size_diff_7)

## Latency to Choose ----
latency_diff_7 <-experimental_data %>%
  filter(Food_Deprivation_Length == 7) %>%
  dplyr::select(Experiment_ID, Choice, Choice_1, Food_Deprivation_Length, Acclimation, latency_2_choose)%>%
  drop_na(latency_2_choose)

latency_diff_7$Choice_1 <- as.factor(latency_diff_7$Choice_1)
latency_diff_7$Choice <- as.factor(latency_diff_7$Choice)

ggplot(latency_diff_7, aes(x = Choice_1, y = latency_2_choose, colour = as.factor(Choice)))+
  geom_point()+
  geom_boxplot()

lat.aov2 <- aov(latency_2_choose ~ Choice + Choice_1, data = latency_diff_7)
summary(lat.aov2)

lat.aov3 <- aov(latency_2_choose ~ Choice * Choice_1, data = latency_diff_7)
summary(lat.aov3) #interaction is not significant, just using the additive model (aov2)

lat_aov2_tukey <- TukeyHSD(lat.aov2, which = "Choice_1")
pairwise.t.test(latency_diff_7$latency_2_choose, latency_diff_7$Choice_1,
                p.adjust.method = "BH")

#homogeneity of variance
plot(lat.aov2, 1)
car::leveneTest(latency_2_choose ~ Choice * Choice_1, data = latency_diff_7)
aov_residuals <- residuals(object = lat.aov2)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals) #not normal

#unbalanced designs
car::Anova(lat.aov3, type = "III")

hist(latency_diff_7$latency_2_choose)

#non-parametric option
# do pairwise Wilcoxon test for pairwise comparisons between groups
lat_wilcox_7 <- latency_diff_7 %>%
  group_by(Choice_1) %>%
  pairwise_wilcox_test(latency_2_choose ~ Choice)

# do Kruskal Wallis test to see whether or not there is statistically significant difference between three or more groups
lat_kw_7 <- compare_means(latency_2_choose ~ Choice, latency_diff_7, group.by = c("Choice_1"), method="kruskal") 

# 3-days Food Deprived ----
## Data Summary ----
experimental_data_sum_3 <- experimental_data %>% 
  filter(Food_Deprivation_Length == 3) %>%
  group_by(Choice_1) %>%
  summarise(total_trial = n(),
            num_sucesses = sum(Choice))%>%
  mutate(p_value = NA, CI_95_upper = NA, CI_95_lower = NA, 
         prob_sucess = num_sucesses/total_trial)

## Binomial Test ----
for(i in 1:nrow(experimental_data_sum_3)) {
  analysis_result <- binom.test(x = experimental_data_sum_3$num_sucesses[i],
                                n = experimental_data_sum_3$total_trial[i],
                                p = 0.5,
                                alternative = "two.sided")
  
  experimental_data_sum$p_value[i] = as.numeric(analysis_result[3])
  experimental_data_sum$CI_95_lower[i] = as.numeric(unlist(analysis_result[4])[1])
  experimental_data_sum$CI_95_upper[i] = as.numeric(unlist(analysis_result[4])[2])
  
  assign("experimental_data_sum_3", experimental_data_sum, envir = .GlobalEnv)
}

## Anemone Size ----
size_diff_3 <- experimental_data %>%
  dplyr::select(Experiment_ID, Choice, Choice_1, Food_Deprivation_Length, radius_chosen, radius_other) %>%
  filter(Food_Deprivation_Length == 3) %>%
  mutate(radius_diff = radius_chosen-radius_other)

hist(size_diff_3$radius_diff, breaks = 10)

size_diff_3 %>% identify_outliers(radius_diff)
size_diff_3 %>% shapiro_test(radius_diff) 
size_diff_3 %>% t_test(radius_diff ~ 1, mu = 0)
size_diff_3 %>% cohens_d(radius_diff ~ 1, mu = 0)

#checking with an anova
size_diff_3$Choice_1 <- as.factor(size_diff_3$Choice_1)
levels(size_diff_3$Choice_1)

ggplot(size_diff_3, aes(x = Choice_1, y = radius_diff))+
  geom_point()+
  geom_boxplot()

size.aov_3 <- aov(radius_diff ~ Choice_1, data = size_diff_3)
size_aov_tukey_3 <- TukeyHSD(size.aov_3)

#homogeneity of variances
plot(size.aov, 1)
car::leveneTest(radius_diff ~ Choice_1, data = size_diff_3) #variance across groups is statistically significant
oneway.test(radius_diff ~ Choice_1, data = size_diff_3)
pairwise.t.test(size_diff_3$radius_diff, size_diff_3$Choice_1,
                p.adjust.method = "BH", pool.sd = FALSE)
#normality
plot(size.aov, 2)
aov_residuals <- residuals(object = size.aov)
shapiro.test(x = aov_residuals)

size_kruskal_3 <- kruskal.test(radius_diff ~ Choice_1, data = size_diff_3)

## Latency to Choose ----
latency_diff_3 <-experimental_data %>%
  filter(Food_Deprivation_Length == 3) %>%
  dplyr::select(Experiment_ID, Choice, Choice_1, Food_Deprivation_Length, latency_2_choose)%>%
  drop_na(latency_2_choose)

latency_diff_3$Choice_1 <- as.factor(latency_diff_3$Choice_1)
latency_diff_3$Choice <- as.factor(latency_diff_3$Choice)

ggplot(latency_diff_3, aes(x = Choice_1, y = latency_2_choose, colour = as.factor(Choice)))+
  geom_point()+
  geom_boxplot()

lat.aov2 <- aov(latency_2_choose ~ Choice + Choice_1, data = latency_diff_3)
summary(lat.aov2)

lat.aov3 <- aov(latency_2_choose ~ Choice * Choice_1, data = latency_diff_3)
summary(lat.aov3) #interaction is not significant, just using the additive model (aov2)

lat_aov2_tukey <- TukeyHSD(lat.aov2, which = "Choice_1")
pairwise.t.test(latency_diff_3$latency_2_choose, latency_diff_3$Choice_1,
                p.adjust.method = "BH")

#homogeneity of variance
plot(lat.aov2, 1)
car::leveneTest(latency_2_choose ~ Choice * Choice_1, data = latency_diff_3)
aov_residuals <- residuals(object = lat.aov2)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals) #not normal

#unbalanced designs
car::Anova(lat.aov3, type = "III")

hist(latency_diff_3$latency_2_choose)

#non-parametric option
# do pairwise Wilcoxon test for pairwise comparisons between groups
lat_wilcox_3 <- latency_diff_3 %>%
  group_by(Choice_1) %>%
  pairwise_wilcox_test(latency_2_choose ~ Choice)

# do Kruskal Wallis test to see whether or not there is statistically significant difference between three or more groups
lat_kw_3 <- compare_means(latency_2_choose ~ Choice, latency_diff_3, group.by = c("Choice_1"), method="kruskal") 

# Save dfs ----
write_df_2_csv <- function(output_path,
                           output_csv_name,
                           exp_type = NULL,
                           input_df){
  today <- Sys.Date()
  currentDate <- format(today, format="%Y%m%d")
  FileName <- paste(exp_type,
                    output_csv_name,
                    currentDate,
                    sep="_")
  as.character(FileName)
  csvFileName <- paste(output_path, FileName, ".csv", sep="")
  csvFileName <- as.character(csvFileName)
  fwrite(input_df, file=csvFileName)
}

if(save_analysis_as_csv == TRUE) {
  ## Save cleaned data as a csv
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "binomial_test_results_3",
                 input_df = experimental_data_sum_3)
  
  capture.output(size_aov_tukey_3, file = paste(analysis_path,"2AC_sizedifference_anova_tukey_3.txt", sep = ""))
  
  capture.output(size_kruskal_3, file = paste(analysis_path,"2AC_sizedifference_kruskaltest_3.txt", sep = ""))
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "latency_wilcox_3",
                 input_df = lat_wilcox_3)
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "latency_kruskal_3",
                 input_df = lat_kw_3)
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "binomial_test_results_7",
                 input_df = experimental_data_sum_7)
  
  capture.output(size_aov_tukey_7, file = paste(analysis_path,"2AC_sizedifference_anova_tukey_7.txt", sep = ""))
  
  capture.output(size_kruskal_7, file = paste(analysis_path,"2AC_sizedifference_kruskaltest_7.txt", sep = ""))
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "latency_wilcox_7",
                 input_df = lat_wilcox_7)
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "latency_kruskal_7",
                 input_df = lat_kw_7)
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "size_diff_7",
                 input_df = size_diff_7)
  
  write_df_2_csv(output_path = analysis_path,
                 exp_type = "2AC",
                 output_csv_name = "latency_7",
                 input_df = latency_diff_7)
}
