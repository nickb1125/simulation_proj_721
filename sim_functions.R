# simulation functions

simulate <- function(trial_num = 1, pct_exposed, risk_control, risk_treatment, FPR, FNR) {
  samples <- get_true_and_bias_samples(pct_exposed = pct_exposed, risk_control = risk_control, risk_treatment = risk_treatment, FPR = FPR, FNR = FNR)
  
  rr_estimate <- estimate_rr(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  rd_estimate <- estimate_rd(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  or_estimate <- estimate_or(exposure = samples$exposure_bias, outcome = samples$outcome_true)
  
  rr_non_bias_estimate <- estimate_rr(exposure = samples$exposure_true, outcome = samples$outcome_true)
  rd_non_bias_estimate <- estimate_rd(exposure = samples$exposure_true, outcome = samples$outcome_true)
  or_non_bias_estimate <- estimate_or(exposure = samples$exposure_true, outcome = samples$outcome_true)
  
  rr_true <- risk_control / risk_treatment
  rd_true <- risk_control - risk_treatment
  or_true <- (risk_control / (1 - risk_control)) / (risk_treatment / (1 - risk_treatment))
  
  metric_df <- data.frame('RR_estimate' = rr_estimate, 'RD_estimate' = rd_estimate,'OR_estimate' = or_estimate,
                          'RR_true' = rr_true, 'RD_true' = rd_true, 'OR_true' = or_true,
                          'RR_non_bias_estimate' = rr_non_bias_estimate, 'RD_non_bias_estimate' = rd_non_bias_estimate, 'OR_non_bias_estimate' = or_non_bias_estimate)
  return(metric_df)
}


simulate_multiple <- function(simulation_n, pct_exposed, risk_control, risk_treatment, FPR, FNR) {
  multiple_sim <- sapply(c(1:simulation_n), simulate, pct_exposed = pct_exposed, risk_control = risk_control,
                         risk_treatment= risk_treatment, FPR = FPR, FNR = FPR) %>% t() %>% `mode<-`('numeric') %>% as.data.frame()
  return(multiple_sim)
}