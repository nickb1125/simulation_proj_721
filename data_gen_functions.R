binary_vector_biaser <- function(binary_vector, misclass_rate_pos, misclass_rate_neg) { # Generates biased verison of true sample
  biased_vec <- lapply(binary_vector, function(x) { # If sample = 1 change with prob FNR, if sample = 0 change with probn FPR
    ifelse(x == 1, rbinom(n = 1, size = 1, prob = (1 - misclass_rate_pos)), rbinom(n = 1, size = 1, prob = misclass_rate_neg))
  })
  return(unlist(biased_vec))
}

get_true_and_bias_samples <- function(pct_exposed, risk_control, risk_treatment, FPR, FNR) { # get both true samples and biased samples in one df
  
  # get true samples
  exposure_true <- rbinom(n = 500, size = 1, prob = pct_exposed)
  
  outcome_true <- unlist(lapply(exposure_true, function(x) {ifelse(x == 1, rbinom(n = 1, size = 1, prob = risk_treatment),
                                                                   rbinom(n = 1, size = 1, prob = risk_control))}))
  
  # get biased samples
  exposure_bias <- binary_vector_biaser(binary_vector = exposure_true, misclass_rate_pos = FNR, misclass_rate_neg = FPR)
  
  # put together
  
  data_samples <- data.frame('exposure_true' = exposure_true, 'outcome_true' = outcome_true,
                             'exposure_bias' = exposure_bias)
  
  
  return(data_samples)
}