# simulation functions

simulate <- function(trial_num = 1, pct_exposed, risk_control, risk_treatment, FPR, FNR) { 
  # Simulate 1 sample of size 500 and get corresponding bias and unbiased estimates
  
  # get bias and unbiased samples
  samples <- get_true_and_bias_samples(pct_exposed = pct_exposed, risk_control = risk_control, 
                                       risk_treatment = risk_treatment, FPR = FPR, FNR = FNR)
  
  # get metrics from samples
  rr_estimate <- estimate_rr(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  rd_estimate <- estimate_rd(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  or_estimate <- estimate_or(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  
  rr_non_bias_estimate <- estimate_rr(exposure = samples$exposure_true, outcome = samples$outcome_true)
  rd_non_bias_estimate <- estimate_rd(exposure = samples$exposure_true, outcome = samples$outcome_true)
  or_non_bias_estimate <- estimate_or(exposure = samples$exposure_true, outcome = samples$outcome_true)
  
  rr_true <- risk_control / risk_treatment
  rd_true <- risk_control - risk_treatment
  or_true <- (risk_control / (1 - risk_control)) / (risk_treatment / (1 - risk_treatment))

  
  # comprise in dataframe with 1 row
  metric_df <- data.frame('RR_estimate' = rr_estimate, 'RD_estimate' = rd_estimate,'OR_estimate' = or_estimate,
                          'RR_true' = rr_true, 'RD_true' = rd_true, 'OR_true' = or_true,
                          'RR_non_bias_estimate' = rr_non_bias_estimate, 'RD_non_bias_estimate' = rd_non_bias_estimate, 
                          'OR_non_bias_estimate' = or_non_bias_estimate) 
  return(metric_df)
}


simulate_multiple <- function(simulation_n, pct_exposed, risk_control, risk_treatment, FPR, FNR) {
  # Complete simulation_n number of trials with 500 patients and get dataframe of size simulation_n representing each trial with all metrics

  multiple_sim <- sapply(c(1:simulation_n), simulate, pct_exposed = pct_exposed, risk_control = risk_control,
                         risk_treatment= risk_treatment, FPR = FPR, FNR = FNR) %>% t() %>% `mode<-`('numeric') %>% as.data.frame() 
  return(multiple_sim)
}

calc_how_far_off <- function(pct_exposed, risk_control, risk_treatment, FPR, FNR) { # get how far off metrics are expected to be
  pct_control <- 1 - pct_exposed
  
  positive_exposures_miclassified_positive_outcome <- (pct_exposed*FNR)*risk_treatment # pct people who are truly exposed, misclassified as control, and positive for outcome
  negative_exposures_miclassified_positive_outcome <- (pct_control*FPR)*risk_control # pct people who are in control, misclassified as exposed, and positive for outcome 
  
  
  # pct positive true controls  - pct positive true controls who are misclassified + pct positive true exposed who are misclassified
  risk_control_bias <- risk_control - negative_exposures_miclassified_positive_outcome + positive_exposures_miclassified_positive_outcome
  
  # pct positive true exposed  - pct positive true exposed who are misclassified + pct positive true control who are misclassified
  risk_exposed_bias <- risk_treatment - positive_exposures_miclassified_positive_outcome + negative_exposures_miclassified_positive_outcome
  
  true_rr <- risk_control / risk_treatment
  true_rd <- risk_control - risk_treatment
  true_or <- (risk_control / (1-risk_control)) / (risk_treatment / (1-risk_treatment))
  
  bias_rr <- risk_control_bias / risk_exposed_bias
  bias_rd <- risk_control_bias - risk_exposed_bias
  bias_or <- (risk_control_bias / (1-risk_control_bias)) / (risk_exposed_bias / (1-risk_exposed_bias))
  
  #get true - biased
  
  diff_rr <- true_rr - bias_rr
  diff_rd <- true_rd - bias_rd
  diff_or <- true_or - bias_or
  
  misclass_rate <- positive_exposures_miclassified_positive_outcome + negative_exposures_miclassified_positive_outcome
  
  how_off <- data.frame(FPR = FPR, FNR = FNR, diff_rr = diff_rr, diff_rd = diff_rd, diff_or = diff_or)
  return(how_off)
}




how_far_this_row <- function(grid, row_index, pct_exposed, risk_control, risk_treatment) { # runs how far funciton on all rows of grid search df
  this_row <- grid[row_index,]
  pct_exposed <- as.numeric(this_row[1])
  risk_exposed <- as.numeric(this_row[2])
  risk_control <- as.numeric(this_row[3])
  FPR <- as.numeric(this_row[4])
  FNR <- as.numeric(this_row[5])
  
  how_far <- calc_how_far_off(pct_exposed = pct_exposed, risk_control = risk_control, risk_treatment = risk_exposed, 
                              FPR = FPR, FNR = FNR)
  return(how_far)
}

