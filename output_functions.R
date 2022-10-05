# graphing functions

graph_simulation_metric_bias <- function(sim) {
  rr_graph <- ggplot(data = sim) + geom_histogram(aes(x = RR_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(RR_true), color = 'True')) +
    geom_vline(aes(xintercept = mean(RR_estimate), color = 'Simulation')) +
    geom_density(aes(x = RR_non_bias_estimate, color = 'True')) + theme(legend.position="none")
  
  rd_graph <- ggplot(data = sim) + geom_histogram(aes(x = RD_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(RD_true), color = 'True')) +
    geom_vline(aes(xintercept = mean(RD_estimate), color = 'Simulation'))+
    geom_density(aes(x = RD_non_bias_estimate, color = 'True')) + theme(legend.position="none")
  
  or_graph <- ggplot(data = sim) + geom_histogram(aes(x = OR_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(OR_true), color = 'True')) +
    geom_vline(aes(xintercept = mean(OR_estimate), color = 'Simulation')) +
    geom_density(aes(x = OR_non_bias_estimate, color = 'True')) 
  
  legend <- get_legend(or_graph)
  
  or_graph <- or_graph + theme(legend.position="none")
  
  ret <- rr_graph + rd_graph + or_graph + legend
  
  return(ret)
}

simulation_info_table <- function(simulation_n, pct_exposed, risk_control, risk_treatment, FPR, FNR) {
  attributes <- c('Number of Simulations', 'Prior Probability of Exposure', 
                  'True Exposure Risk', 'True Control Risk', 'False Positive Rate', 'True Positive Rate')
  values <- as.character(c(simulation_n, pct_exposed, risk_control, risk_treatment, FPR, FNR))
  data.frame(Attributes = attributes, Values = values) %>% gt() %>%
    tab_header(
      title = md("Simulation Settings"),
    )
}


simulation_confusion_mat <- function(pct_exposed, risk_control, risk_treatment, FPR, FNR) {
  expected_exposed <- 500*(pct_exposed) 
  expected_control <- 500*(1 - pct_exposed)
  
  expected_FN <- expected_exposed*FNR
  expected_TP <- expected_exposed - expected_FN
  
  expected_FP <- expected_control*FPR
  expected_TN <- expected_control - expected_FP
  
  confusion <- data.frame(matrix(nrow = 2, ncol = 3)) %>%
    `colnames<-`(c('Predicted', 'Total Positive', 'Total Negative'))%>%
    mutate('Predicted' = c('Positive Predicted', 'Negative Predicted'),
           'Total Positive' = c(expected_TP, expected_FN),
           'Total Negative' = c(expected_FP, expected_TN)) %>%
    gt() %>%
    tab_header(
      title = md("Expected Confusion Matrix by Exposure and Control Groups"),
    )
  
  confusion
}


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}