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
                         risk_treatment= risk_treatment, FPR = FPR, FNR = FPR) %>% t() %>% `mode<-`('numeric') %>% as.data.frame() 
  return(multiple_sim)
}
