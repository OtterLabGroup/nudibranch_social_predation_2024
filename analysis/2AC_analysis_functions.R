###### 2-Alternative Choice Test Functions ######

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

bayesian_binom_test <- function(n_draws, observed_data_vector){
  
  #get the number of trials
  n_trials = length(observed_data_vector)
  
  # Here you sample n_draws draws from the prior
  prior <- rbeta(n_draws, shape1 = 1, shape2 = 1)
  
  # generative model (likelihood)
  generative_model <- function(prior_prob) {
    rbinom(n=1, size = n_trials, prob = prior_prob)
  }
  
  # Approximate Bayesian Computation
  
  # simulate data using the parameters from the prior and the generative model
  sim_data <- rep(NA, n_draws)
  for(i in 1:n_draws) {
    sim_data[i] <- generative_model(prior_prob = prior[i])
  }
  
  # Here you filter off all draws that do not match the data. tolerance e = 0
  posterior <- prior[sim_data == sum(observed_data_vector)] 
  
  hist(posterior, breaks = 20, xlim = c(0,1)) # Eyeball the posterior
  abline(v = 0.5, col = "red")
  posterior_size <- length(posterior)
  
  if(posterior_size < 1000) {
    warning("you should draw additional data, the filtered posterior was less that 1,000")
  }
  
  # Summary statistics of posterior:mean, median posterior, 95% quantile interval.
  mean = mean(posterior)
  median = median(posterior)
  CI_95 = quantile(posterior, c(0.025, 0.975))
  
  #probability that the rate of success is greater than 0.5
  prob_over_50 = sum(posterior > 0.5)/length(posterior)
  prob_under_50 = sum(posterior < 0.5)/length(posterior)
  
  bayesian_binom_test_result <- cbind(mean, median, CI_95_lower = CI_95[1], CI_95_upper = CI_95[2], prob_over_50, prob_under_50)
  
  assign("bayesian_binom_test_result", bayesian_binom_test_result, envir = .GlobalEnv)
  return(bayesian_binom_test_result)
}

