library(dplyr)
library(ggpubr)
library(gt)
library(ggplot2)
library(scatterplot3d)

###################################################################### Project Plan ##################################################################################################

            # Steps:

            #     Single Simulation
            #                   1. Simulate true exposures 
            #                   2. Calculate true outcomes based on exposures (if exposure == 1 use risk_exposure, if exposure == 0 use risk_control)
            #                   3. Get biased exposure vector by biasing true exposure vector (change with prob (1-FNR) if exposure == 1, change with prob 1-FPR if exposure == 0)
            #                   4. Calculate biased relative risk, odds ratio, and risk difference based on biased outcomes with true exposures
            #
            #     Multiple Simulation
            #                   1. Rerun steps 1 to 5 for single simulation 1000 times; collect samples of each metric for all 1000 trials
            #                   2. Plot distributions (histograms + mean lines) for simulated bias metrics as well as the true distributions
            #                   3. Create expected confusion matrix based on input TPR, FPR



            # Functions Needed: 

            #                   1. Vector Biaser (To bias true exposure values at different FPR or FNR)
            #                   2. True Sample Get: (Use binomial simulation of prior exposure prob to get true exposure labels)
            #                   3. Bias Sample Get: (Use vector biaser on true sample to get bias samples based on FPR and FNR)
            #                   4. Estimators: (Define functions to get risk, RR, OR, and RD from vectors for exposure and outcome)
            #                   5. Simulate: (Simulate 1 trial from prior exposure risk, risk_trt, risk_control, etc)
            #                   6. Simulate All: (Uses single simulations in lapply to do all simulations)
            #                   7. how_off: get exact expected differences for each metric based on settings for 3d plots
            #                   8. Graphing functions for histograms, true distributions, expected confusion tables based on TPR, FPR, N
    

            # Shiny Layout: 

            #                   1. Sliders for inputs of TPR, FPR, Risk Exposure, Risk Control (0n left)
            #                   2. Table for current simulation settings (On top right)
            #                   3. Expected confusion table (middle right)
            #                   4. Histograms with density plots for each metric (bottom right)
            #                   5. 3D plots (FPR, FNR, bias of metrics) based on inut exposure percent, risks

######################################################################################################################################################################################


library(dplyr)
library(gt)
library(ggpubr)

source('measure_functions.R')
source('data_gen_functions.R')
source('sim_functions.R')
source('output_functions.R')





ui <- shinyUI(fluidPage(
  
  titlePanel("Exposure Misclassification Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Simulate button
      submitButton("Simulate"),
      
      #Input sliders for all variables
      
      sliderInput("pct_exposed", "Prior Exposure Liklihood:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01,
                  animate = TRUE),
      
      sliderInput("risk_control", "Risk Control Group:",
                  min = 0, max = 1,
                  value = 0.2, step = 0.01,
                  animate = TRUE),
      
      sliderInput("risk_treatment", "Risk Exposure Group:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01,
                  animate = TRUE),
      
      sliderInput("FPR", "False Positive Rate:",
                  min = 0, max = 0.5,
                  value = 0, step = 0.01,
                  animate = TRUE),
      
      sliderInput("FNR", "False Negative Rate:",
                  min = 0, max = 0.5,
                  value = 0, step = 0.01,
                  animate = TRUE),
    ),
    
    #outputs
    
    mainPanel(  
      tabsetPanel(
        tabPanel("Tables", br(), gt_output( 'settings' ),
                 gt_output( 'confusion' )),
        tabPanel("Simulated", br(), plotOutput( 'histograms' )),
        tabPanel("Expected Metric Bias Plots", br(), plotlyOutput("threeD_plots"), htmlOutput("explain") )
      ),
      
    )
  )
))


server <- function(input, output) {
  # Observe function to make all objects reactive
  observe({ 
    
    list(input$pct_exposed, input$risk_control, input$risk_treatment, input$FPR, input$FNR)
    
    # Output settings table
    output$settings = render_gt({
      simulation_info_table(simulation_n = 500, simulation_sample_size = 500, pct_exposed = input$pct_exposed, 
                            risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                            FPR = input$FPR, FNR = input$FNR)
      
    })
    
    # Output expected confusion table
    output$confusion = render_gt({
      simulation_confusion_mat(pct_exposed = input$pct_exposed, 
                               risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                               FPR = input$FPR, FNR = input$FNR)
      
      
    })
    
    # Output simulation results for 500 trials  with 500 people in each trial
    output$histograms = renderPlot({
      sim <- simulate_multiple(simulation_n = 500, pct_exposed = input$pct_exposed, 
                               risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                               FPR = input$FPR, FNR = input$FNR)
      graph_simulation_metric_bias(sim)
    
    
  })
    
    output$threeD_plots <- renderPlotly({
      get_misclass_plots(input$pct_exposed, input$risk_control, input$risk_treatment)
    })
    
    output$explain <- renderText({"\n <b>It is clear from both the simulations and from the expected difference plots that misclassification rate has a positive relationship with bias for all of our metrics.
    From these plots we see that this relationship is linear for risk difference and non-linear for both odds ratios and relative risk. By adjusting the sliders for risk and percent exposed,
    one can see that the degree of non-linearity for these variables bias is largely dependent on the risk of prior exposure as well as on the risk in the exposure and control groups.
    We see that these non-linearities in bias can partially be attributed to the seperation of FPR and FNR, since these rates affect total mislcassifcation rate differently
    dependent on the prior exposure likliehood. </b>"})
  
})
}




# Run the application 
shinyApp(ui = ui, server = server)
