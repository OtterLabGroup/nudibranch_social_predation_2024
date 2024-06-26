##### PPR Analysis Functions #####
# compute the statistics from the real data ----
real.stats <- function(df){
  return(list(
    meanmax = mean(df$max_group_size, na.rm = TRUE),
    meanmean = mean(df$avg_grp_size, na.rm = TRUE),
    alone = sum(df$num_not_feeding, na.rm = TRUE)
  ))
}

# p-value computations ---- 
#(uses real or simulated data to then simulate sets of data and compute a p-value)
p.val.helper <- function(observed, simu.data, direction){
  if (direction == 1){
    p.val <- mean(observed <= simu.data)
  }else if(direction == 2){
    p.val <- mean(observed >= simu.data)
  }else if(direction == 3){
    p.val <- min(2 * min(mean(observed <= simu.data), mean(observed >= simu.data)), 1)
  }
  return(p.val)
}

get.p.value <- function(obs.list, slug.num, prey.num, nonfeeding.num, N, method2use, direction = c(1, 1, 2, 1)){
  null.dist <- replicate(N, slug_simulation(slug.num, prey.num, nonfeeding.num, method = method2use), simplify = FALSE)
  
  # simulated data
  simu.meanmax <- rep(NA, N)
  simu.meanmean <- rep(NA, N)
  simu.alone <- rep(NA, N)
  for (i in 1:N){
    simu.meanmax[i] <- null.dist[[i]]$meanmax.size
    simu.meanmean[i] <- null.dist[[i]]$meanmean.size
    simu.alone[i] <- null.dist[[i]]$alone
  }
  
  # 1 is lower tail, 2 is upper tail, 3 is the default two-tailed
  p.val <- c(
    p.val.helper(obs.list$meanmax, simu.meanmax, direction[2]),
    p.val.helper(obs.list$meanmean, simu.meanmean, direction[2]),
    p.val.helper(obs.list$alone, simu.alone, direction[3])
  )
  
  return(p.val)
}

## real data ----
get.real.p.value <- function(real.data, num.slug, num.prey, num.nonfeeding, N, method2use = 'equal'){
  real.p.vals <- data.frame(t(sapply(list(real.data), get.p.value, num.slug, num.prey, num.nonfeeding, N, method2use)))
  colnames(real.p.vals) <- c('MeanMax', 'MeanMean', 'Alone')
  return(real.p.vals)
}

## simulated data ----

# compute the power of the test ----
rejection_rates_2 <- function(df){
  new.labels <- c('MeanMax', 'MeanMean', 'Alone')
  df[1, ] <- apply(df, 2, function(x) mean(x <= 0.05))
  colnames(df) <- new.labels
  return(df)
}

# function for alpha computation ----
expected.table <- function(n, alpha){
  return(sum(alpha / (1:n + alpha - 1)))
}

bisec.solver <- function(n, a, b, theory){ 
  # a and b are upper and lower thresholds for the initialization of the alpha parameter
  c <- mean(c(a, b)) #midpoint
  # Bayesian statistics: compute expected from distributions --> theoretical expected value
  c.value <- expected.table(n, c) 
  if (abs(theory - c.value) < 1e-8){ #checks for the threshold difference between expected and theory
    return(c)
  }else{ #if greater than threshold, iterates again
    if((theory - c.value) > 1e-8){
      bisec.solver(n, c, b, theory)
    }else{
      bisec.solver(n, a, c, theory)
    }
  }
}

# Chinese restaurant process function ----
#takes input # of slugs, # of preys and parameter alpha, outputs a vector of indices of preys on which the slugs feed
CRP = function(nDnr, num.Tbl, alpha){
  vDnrTbl <- rep(0, nDnr)
  vDnrTbl[1] <- 1
  for (dnr in 2:length(vDnrTbl)) {
    
    # compute occupation probabilities for current diner
    vOcc <- table(vDnrTbl[1:(dnr-1)])
    if (length(vOcc) <= num.Tbl-1){
      vProb <- c(vOcc, alpha) / (dnr - 1 + alpha)
      # add table label to diner
      nTbl <- as.numeric(names(vOcc)[length(vOcc)])  # avoid overhead of finding max of possibly large vector
      vDnrTbl[dnr] <- sample.int(nTbl+1, size=1, prob=vProb)
    }else{
      vProb <- c(vOcc) / (dnr - 1 + alpha)
      # add table label to diner
      nTbl <- as.numeric(names(vOcc)[length(vOcc)])  # avoid overhead of finding max of possibly large vector
      vDnrTbl[dnr] <- sample.int(nTbl, size=1, prob=vProb)
    }
  }
  return(vDnrTbl)
}

# Data simulation ----- 
#takes input # of slugs, # of preys, non-feeding # of slug, simulation method and alpha, outputs four statistics
slug_simulation <- function(slug.num, prey.num, nonfeeding.num, method, alpha=1){
  # slug.num and prey.num are scalars while nonfeeding.num is a vector
  # method is the way that the function simulates the sampling process, currently 'equal' or 'unequal'
  trial.num <- length(nonfeeding.num)
  feeding.num <- slug.num - nonfeeding.num
  
  max.groupSize <- rep(NA, trial.num)
  mean.groupSize <- rep(NA, trial.num)
  num.feedingAlone <- rep(NA, trial.num)
  
  for (trial in 1:trial.num){
    if (method == 'equal'){
      #randomly sample with replacement 1:prey number, samples taken is determined by number of feeding slugs
      prey.index <- sample(1:prey.num, size = feeding.num[trial], replace = TRUE) 
    }else if(method == 'unequal'){
      #biases the probabilities of each prey
      prey.index <- sample(1:prey.num, size = feeding.num[trial], prob = 1:prey.num, replace = TRUE)
    }else if(method == 'CRP'){
      #use the CRP process 
      prey.index <- CRP(slug.num, prey.num, alpha)
    }else if(method == 'square'){
      #biasing is stronger than unequal, where prey are more attractive 
      prey.index <- sample(1:prey.num, size = feeding.num[trial], prob = (1:prey.num)^2, replace = TRUE)
    }
    
    trial.groupSize <- as.numeric(table(prey.index))
    
    max.groupSize[trial] <- max(trial.groupSize)
    mean.groupSize[trial] <- mean(trial.groupSize)
    num.feedingAlone[trial] <- sum(trial.groupSize == 1)
  }
  
  mean.max.groupSize <- mean(max.groupSize)
  mean.mean.groupSize <- mean(mean.groupSize)
  sum.feedingalone <- sum(num.feedingAlone)
  
  return(
    list(
      meanmax.size = mean.max.groupSize,
      meanmean.size = mean.mean.groupSize, 
      alone = sum.feedingalone
    )
  )
}

# save outputs to csv ----
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