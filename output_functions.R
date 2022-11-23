# graphing functions

graph_simulation_metric_bias <- function(sim) {
  
  ## Plot density for true and biased simualtions RR
  rr_graph <- ggplot(data = sim) + geom_histogram(aes(x = RR_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(RR_true), color = 'True Value')) +
    geom_vline(aes(xintercept = mean(RR_estimate), color = 'Biased Simulation')) +
    geom_density(aes(x = RR_non_bias_estimate, color = 'True Simulation')) + theme(legend.position="none") + xlab('Relative Risk Estimate') + 
    ylab('Density')
  
  ## Plot density for true and biased simualtions RD
  rd_graph <- ggplot(data = sim) + geom_histogram(aes(x = RD_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(RD_true), color = 'True Value')) +
    geom_vline(aes(xintercept = mean(RD_estimate), color = 'Biased Simulation'))+
    geom_density(aes(x = RD_non_bias_estimate, color = 'True Simulation')) + theme(legend.position="none") + xlab('Relative Difference Estimate') + 
    ylab('Density')
  
  ## Plot density for true and biased simulations OR
  or_graph <- ggplot(data = sim) + geom_histogram(aes(x = OR_estimate, y = ..density..), bins = 30) + 
    geom_vline(aes(xintercept = mean(OR_true), color = 'True Value')) +
    geom_vline(aes(xintercept = mean(OR_estimate), color = 'Biased Simulation')) +
    geom_density(aes(x = OR_non_bias_estimate, color = 'True Simulation')) + xlab('Odds Ratio Estimate') + 
    ylab('Density') +labs(colour="Simulation Type") +
    theme(legend.key.size = unit(2, 'cm'))
  
  # Get universal legend
  legend <- get_legend(or_graph)
  or_graph <- or_graph + theme(legend.position="none") 
  
  # Arange all in 1 plot
  ret <- ggarrange(rr_graph, rd_graph, or_graph, legend)
  
  ret <- annotate_figure( ret , top = text_grob('Simulated True and Biased Results', 
                                        color = "black", face = "bold", size = 14))
  
  return(ret)
}

simulation_info_table <- function(simulation_n, simulation_sample_size, pct_exposed, risk_control, risk_treatment, FPR, FNR) { # Function to get simulation settings table
  attributes <- c('Number of Simulations', 'Single Simulation Sample Size', 'Prior Probability of Exposure', 
                  'True Exposure Risk', 'True Control Risk', 'False Positive Rate', 'True Positive Rate')
  values <- as.character(c(simulation_n, simulation_sample_size, pct_exposed, risk_control, risk_treatment, FPR, FNR))
  data.frame(Attributes = attributes, Values = values) %>% gt() %>%
    tab_header(
      title = md("Simulation Settings")
    )
}


simulation_confusion_mat <- function(pct_exposed, risk_control, risk_treatment, FPR, FNR) { # Get expected confusionmatrix
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


get_legend<-function(myggplot){ # get legend from grobs object
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



grid_search_how_off <- function(pct_exposed, risk_control, risk_treatment){ # execute search grid
  FPR_grid <- seq(0, 0.5, by = 0.01)
  FNR_grid <- seq(0, 0.5, by = 0.01)
  
  grid <- expand.grid(pct_exposed = c(pct_exposed), risk_exposed = c(risk_treatment), risk_control = c(risk_control), 
                      FPR = FPR_grid, FNR = FNR_grid)
  
  max_row <- nrow(grid)
  results <- lapply(seq(1, max_row, by = 1), how_far_this_row, grid = grid)
  results <- do.call('rbind', results)
  return(results %>% unique())
}


get_misclass_plots <- function(pct_exposed, risk_control, risk_treatment) { # get 3d plots based on input pct_exposed, risk_control, risk_treatment with fpr and fnr
  contour_df <- grid_search_how_off(pct_exposed = pct_exposed, risk_control = risk_control, risk_treatment = risk_treatment)
  p1 <- plot_ly(x=contour_df$FPR, y=contour_df$FNR, z=contour_df$diff_rr, type="scatter3d", mode="markers",
                marker = list(name = 'rr', color = contour_df$diff_rr, colorscale='RdYIBl', size = 3), scene='scene1')
  p2 <- plot_ly(x=contour_df$FPR, y=contour_df$FNR, z=contour_df$diff_rd, type="scatter3d", mode="markers",
                marker = list(name = 'rd', color = contour_df$diff_rr, colorscale='RdYIBl', size = 3), scene='scene2')
  p3 <- plot_ly(x=contour_df$FPR, y=contour_df$FNR, z=contour_df$diff_or, type="scatter3d", mode="markers",
                marker = list(name = 'or', color = contour_df$diff_rr, colorscale='RdYIBl', size = 3), scene='scene3')
  fig <- subplot(p1, p2, p3) 
  fig <- fig %>% layout(title = "Metric Differences by FPR and FNR (Note: Percent Exposed, Risk Control, and Risk Treatment Used From Settings)",
                        scene = list(name = 'rr', xaxis = list(title = 'False Positive Rate'),
                                     yaxis = list(title = 'False Negative Rate'),
                                     zaxis = list(title = '(True RR) - (Expected RR)'),
                                     camera = list(eye = list(x=-2.5, y=1,  z = 0.4)),
                                     aspectmode = "manual", aspectratio = list(x=1, y=1, z=1)),
                        scene2 = list(name = 'rd', xaxis = list(title = 'False Positive Rate'),
                                      yaxis = list(title = 'False Negative Rate'),
                                      zaxis = list(title = '(True RD) - (Expected RD)'),
                                      camera = list(eye = list(x=-2.5, y=1, z = 0.4)),
                                      aspectmode = "manual", aspectratio = list(x=1, y=1, z=1)),
                        
                        scene3 = list(name = 'or', xaxis = list(title = 'False Positive Rate'),
                                      yaxis = list(title = 'False Negative Rate'),
                                      zaxis = list(title = '(True OR) - (Expected OR)'),
                                      camera = list(eye = list(x=-2.5, y=1, z = 0.4)),
                                      aspectmode = "manual", aspectratio = list(x=1, y=1, z=1)))
  return(fig)
}