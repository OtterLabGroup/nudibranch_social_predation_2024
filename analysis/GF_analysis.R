##### Predator Prey Ratio Analysis #####

# Import Libraries ----
library("dplyr")
library("tidyr")
library("ggplot2")
library("parallel")
library("data.table")

# Initialization ----
project_folder = "H:\\My Drive\\Katz-Lab_Otter-Data\\Projects\\Social_Feeding"
setwd(project_folder)
experimental_data_7 = read.csv(".\\datasets\\predator_prey_ratio\\cleaned_data\\PPR_circle_data_7dFD_20240403.csv")
experimental_data_3 = read.csv(".\\datasets\\state-dependence\\cleaned_data\\PPR_circle_data_3dFD_20240403.csv")
save_analysis_as_csv = TRUE
analysis_path = "H:\\My Drive\\Katz-Lab_Otter-Data\\Projects\\Social_Feeding\\analysis\\"

# Source Functions ----
source("D:\\paper2_sf\\03_analysis\\PPR_analysis_functions.R")

# EXPERIMENT: 7-days FOOD-DEPRIVED
experimental_data <- experimental_data_7
# Initialize model ----
set.seed(68837)
n_replicates = 100000 # number of simulated datasets
n_null_dist <- 100000 # number of simulated datasets for the null distribution
num_slugs <- 8
num_anemones <- 8
num_nonfeeding <- experimental_data$num_not_feeding

# Experimental Data Statistical Analysis ----
# compute the four statistics from the real data
experimental_data_summary_7 <- real.stats(experimental_data)             
# simulate the null distribution, and compute the p-value
experimental_data_p_vals_7 <- get.real.p.value(experimental_data_summary_7, 
                                             num.slug = num_slugs, 
                                             num.prey = num_anemones, 
                                             num.nonfeeding = num_nonfeeding, 
                                             N = n_null_dist)
print(experimental_data_p_vals_7)

# power of the test for the real data
experimental_data_power <- rejection_rates_2(experimental_data_p_vals_7)
print(experimental_data_power)

# Simulation ---- 
# compute the hyperparameter alpha for Dirichlet process (CRP)
occupied_anemones <- as.numeric(apply(experimental_data %>% select(grp_size_8:grp_size_1), 1, sum))
feeding_slugs <- num_slugs - num_nonfeeding
# observed_alpha_7 <-  bisec.solver(mean(feeding_slugs), 0.1, 100, mean(occupied_anemones))

# simulate the null distribution if slugs choose independently
method1 <- 'equal'
simulated_data <- replicate(n_replicates, slug_simulation(slug.num = num_slugs, 
                                                          prey.num = num_anemones, 
                                                          nonfeeding.num = num_nonfeeding,
                                                          method = method1, 
                                                          alpha = observed_alpha_7), simplify = FALSE)

#parallell processing
# cl <- makeCluster(5)
# clusterExport(cl, varlist = c('get.p.value', 'slug_simulation', 'p.val.helper'))
# simulated_p_vals <- data.frame(t(parSapply(cl,
#                                            simulated_data,
#                                         FUN = get.p.value,
#                                         slug.num = num_slugs,
#                                         prey.num = num_anemones,
#                                         nonfeeding.num = num_nonfeeding,
#                                         N = n_null_dist)))
# stopCluster(cl)
# 
# # compute the power and visualize the distribution of p-values
# simulated.p.vals <- rejection_rates_2(simulated_p_vals)
# 
# simulated.p.vals2 <- simulated.p.vals %>% pivot_longer(cols = everything(), names_to = 'Statistics')
# simulated.p.vals2$Statistics <- factor(simulated.p.vals2$Statistics, levels = colnames(simulated.p.vals))
# ggplot(simulated.p.vals2, aes(x = value)) +
#   facet_wrap(~Statistics) + geom_histogram(bins = 20) + xlab('P-value') + ylab('Frequency')


simulated_data_equal_7 <- do.call(rbind.data.frame, simulated_data)
simulated_data_equal_7$model = "equal"

# simulate as a CRP

# method <- 'CRP'
# 
# # simulation
# simulated_data_CRP_7 <- replicate(n_replicates, slug_simulation(slug.num = num_slugs, 
#                                                           prey.num = num_anemones, 
#                                                           nonfeeding.num = num_nonfeeding, 
#                                                           method, 
#                                                           alpha = observed_alpha_7), simplify = FALSE)

# cl <- makeCluster(5)
# clusterExport(cl, varlist = c('method','get.p.value', 'slug_simulation', 'p.val.helper', 'get.p.value'))
# simulated_p_vals_CRP <- data.frame(t(parSapply(cl,
#                                            simulated_data,
#                                            FUN = get.p.value,
#                                            slug.num = num_slugs,
#                                            prey.num = num_anemones,
#                                            nonfeeding.num = num_nonfeeding,
#                                            N = n_null_dist)))
# stopCluster(cl)

# simulated_data_CRP_7 <- do.call(rbind.data.frame, simulated_data_CRP_7)
# simulated_data_CRP_7$model = "CRP"
# 
# simulated_data_7 <- rbind(simulated_data_CRP_7,simulated_data_equal_7)

# EXPERIMENT: 3-days FOOD-DEPRIVED
experimental_data <- experimental_data_3
# Initialize model ----
num_nonfeeding <- experimental_data$num_not_feeding

# Experimental Data Statistical Analysis ----
# compute the four statistics from the real data
experimental_data_summary_3 <- real.stats(experimental_data)             
# simulate the null distribution, and compute the p-value
experimental_data_p_vals_3 <- get.real.p.value(experimental_data_summary_3, 
                                             num.slug = num_slugs, 
                                             num.prey = num_anemones, 
                                             num.nonfeeding = num_nonfeeding, 
                                             N = n_null_dist)
print(experimental_data_p_vals_3)

get.real.p.value(experimental_data_summary_3,
                 num.slug = num_slugs, 
                 num.prey = num_anemones, 
                 num.nonfeeding = num_nonfeeding, 
                 N = n_null_dist,
                 method = 'CRP')


# power of the test for the real data
experimental_data_power <- rejection_rates_2(experimental_data_p_vals_3)
print(experimental_data_power)

# Simulation ---- 
# compute the hyperparameter alpha for Dirichlet process (CRP)
occupied_anemones <- as.numeric(apply(experimental_data %>% select(grp_size_8:grp_size_1), 1, sum))
feeding_slugs <- num_slugs - num_nonfeeding
observed_alpha_3 <-  bisec.solver(mean(feeding_slugs), 0.1, 100, mean(occupied_anemones))

# simulate the null distribution if slugs choose independently
method1 <- 'equal'
simulated_data_3 <- replicate(n_replicates, slug_simulation(slug.num = num_slugs, 
                                                          prey.num = num_anemones, 
                                                          nonfeeding.num = num_nonfeeding, 
                                                          method1, 
                                                          alpha = observed_alpha_3), simplify = FALSE)



# simulated_p_vals <- data.frame(t(sapply(simulated_data, 
#                                         FUN = get.p.value, 
#                                         slug.num = num_slugs, 
#                                         prey.num = num_anemones, 
#                                         nonfeeding.num = num_nonfeeding, 
#                                         N = n_null_dist)))

# cl <- makeCluster(5)
# clusterExport(cl, varlist = c('get.p.value', 'slug_simulation', 'p.val.helper', 'get.p.value'))
# simulated_p_vals_3 <- data.frame(t(parSapply(cl,
#                                            simulated_data,
#                                            FUN = get.p.value,
#                                            slug.num = num_slugs,
#                                            prey.num = num_anemones,
#                                            nonfeeding.num = num_nonfeeding,
#                                            N = n_null_dist)))
# stopCluster(cl)
# 
# # compute the power and visualize the distribution of p-values
# simulated.p.vals_3 <- rejection_rates_2(simulated_p_vals)
# 
# simulated.p.vals2 <- simulated.p.vals %>% pivot_longer(cols = everything(), names_to = 'Statistics')
# simulated.p.vals2$Statistics <- factor(simulated.p.vals2$Statistics, levels = colnames(simulated.p.vals))
# ggplot(simulated.p.vals2, aes(x = value)) +
#   facet_wrap(~Statistics) + geom_histogram(bins = 20) + xlab('P-value') + ylab('Frequency')


simulated_data_equal_3 <- do.call(rbind.data.frame, simulated_data)
simulated_data_equal_3$model = "equal"

# simulate as a CRP

method1 <- 'CRP'

# simulation
simulated_data_CRP_3 <- replicate(n_replicates, slug_simulation(slug.num = num_slugs, 
                                                              prey.num = num_anemones, 
                                                              nonfeeding.num = num_nonfeeding, 
                                                              method1, 
                                                              alpha = observed_alpha_3), simplify = FALSE)

# cl <- makeCluster(5)
# clusterExport(cl, varlist = c('method','get.p.value', 'slug_simulation', 'p.val.helper', 'get.p.value'))
# simulated_p_vals_CRP_3 <- data.frame(t(parSapply(cl,
#                                                simulated_data,
#                                                FUN = get.p.value,
#                                                slug.num = num_slugs,
#                                                prey.num = num_anemones,
#                                                nonfeeding.num = num_nonfeeding,
#                                                N = n_null_dist)))
# stopCluster(cl)

simulated_data_CRP_3 <- do.call(rbind.data.frame, simulated_data_CRP_3)
simulated_data_CRP_3$model = "CRP"

simulated_data_3 <- rbind(simulated_data_CRP_3,simulated_data_equal_3)



# Save dfs ----
if(save_analysis_as_csv == TRUE) {
  ## Save cleaned data as a csv
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "simulated_data_equal",
                 exp_type = "FDL7_PPR",
                 input_df = simulated_data_equal_7)
  
  # write_df_2_csv(output_path = analysis_path,
  #                output_csv_name = "simulated_data_CRP",
  #                exp_type = "FDL7_PPR",
  #                input_df = simulated_data_CRP_7)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "experimental_data_p_vals",
                 exp_type = "FDL7_PPR",
                 input_df = experimental_data_p_vals_7)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "experimental_data_summary",
                 exp_type = "FDL7_PPR",
                 input_df = experimental_data_summary_7)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "simulated_data_equal",
                 exp_type = "FDL3_PPR",
                 input_df = simulated_data_equal_3)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "simulated_data_CRP",
                 exp_type = "FDL3_PPR",
                 input_df = simulated_data_CRP_3)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "experimental_data_p_vals",
                 exp_type = "FDL3_PPR",
                 input_df = experimental_data_p_vals_3)
  
  write_df_2_csv(output_path = analysis_path,
                 output_csv_name = "experimental_data_summary",
                 exp_type = "FDL3_PPR",
                 input_df = experimental_data_summary_3)

}
