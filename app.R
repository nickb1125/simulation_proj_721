library(dplyr)
library(ggpubr)
library(gt)
library(ggplot2)


###################################################################### Project Plan ######################################################################

            # Steps Needed: 1. Vector Biaser (To bias true exposure values at different FPR or FNR)
            #               2. True Sample Get: (Use binomial simulation of prior exposure prob to get true exposure labels)
            #               3. Bias Sample Get: (Use vector biaser on true sample to get bias samples based on FPR and FNR)
            #               4. Estimators: (Define functions to get risk, RR, OR, and RD from vectors for exposure and outcome
            #               5. Simulate: (Simulate 1 trial from prior exposure risk, risk_trt, risk_control, etc)
            #               6. Simulate All: (Uses single simulations in lapply to do all simulations)
            #               7. Utilize graphing functions to get confusion table, histograms for data bias and true distributions

            # Shiny Layout: 1. Sliders (0n right)

##########################################################################################################################################################


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
      
      submitButton("Simulate"),
      
      sliderInput("pct_exposed", "Prior Exposure Likliehood:",
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
    
    mainPanel(  
      gt_output( 'settings' ),
      gt_output( 'confusion' ),
      plotOutput( 'histograms' )
      
    )
  )
))


server <- function(input, output) {
  observe({
    
    output$settings = render_gt({
      simulation_info_table(simulation_n = 500, pct_exposed = input$pct_exposed, 
                            risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                            FPR = input$FPR, FNR = input$FNR)
      
    })
    
    output$confusion = render_gt({
      simulation_confusion_mat(pct_exposed = input$pct_exposed, 
                               risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                               FPR = input$FPR, FNR = input$FNR)
      
      
    })
    
    
    output$histograms = renderPlot({
      sim <- simulate_multiple(simulation_n = 500, pct_exposed = input$pct_exposed, 
                               risk_control = input$risk_control, risk_treatment = input$risk_treatment, 
                               FPR = input$FPR, FNR = input$FNR)
      graph_simulation_metric_bias(sim)
    
    
  })
  
})
}











# Run the application 
shinyApp(ui = ui, server = server)
