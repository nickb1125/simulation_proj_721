# risk and risk measure functions

estimate_risk <- function(sample) {
  risk <- sum(sample) / length(sample)
  return(risk)
}

estimate_rr <- function(exposure, outcome) {
  sample <- data.frame(Exposure = exposure, Outcome = outcome)
  control_sample <- sample$Outcome[sample$Exposure == 0]
  trt_sample <- sample$Outcome[sample$Exposure == 1]
  rr <- estimate_risk(control_sample) / estimate_risk(trt_sample)
  return(rr)
}

estimate_rd <- function(exposure, outcome) {
  sample <- data.frame(Exposure = exposure, Outcome = outcome)
  control_sample <- sample$Outcome[sample$Exposure == 0]
  trt_sample <- sample$Outcome[sample$Exposure == 1]
  rd <- estimate_risk(control_sample) - estimate_risk(trt_sample)
  return(rd)
}

estimate_or <- function(exposure, outcome) {
  sample <- data.frame(Exposure = exposure, Outcome = outcome)
  control_sample <- sample$Outcome[sample$Exposure == 0]
  trt_sample <- sample$Outcome[sample$Exposure == 1]
  or <- ( sum(control_sample) / (length(control_sample) - sum(control_sample)) ) / ( sum(trt_sample) / (length(trt_sample) - sum(trt_sample)) )
  return(or)
}