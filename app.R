#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
library(tidyverse)

future_value = function(intitamount, returnrate, years){
  for (i in 1:years){
    amount1 = intitamount*(1+returnrate)^years
  }
  return(amount1)
}

annuity = function(annualcontribution, returnrate, years){
  for (i in 1:years){
    total = annualcontribution * (((1+returnrate)^years - 1)/returnrate)
  }
  return(total)
}

growing_annuity = function(annualcontribution, returnrate, growthrate, years){
  for ( i in 1:years){
    total1 = annualcontribution* ((((1+returnrate)^years)-((1+growthrate)^years))/(returnrate-growthrate))
  }
  return(total1)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Saving/Investing in 3 differents scenarios"),
  
  fluidRow(
    
    column(4, 
          
             sliderInput(inputId = "intitAmount", 
                         label = "Initial Amount", 
                         min = 0, max = 100000, 
                         value = 1000, 
                         step = 500),
             
             sliderInput(inputId = "annualContribution", 
                         label="Annual Contribution",
                         min = 0, max = 50000, 
                         value = 2000, 
                         step = 500)
    ),
    column(4, offset = 0.5, 
             sliderInput(inputId = "returnRate",
                         label = "Return Rate",
                         min = 0.00, max = 0.20,
                         value = 0.05,
                         step = 0.001),
             
             sliderInput(inputId = "growthrate",
                         label = "Growth Rate",
                         min = 0.00, max = 0.20,
                         value =0.02,
                         step = 0.001, 
                         round = 2) 
    ),
    column(4, offset = 0.5,
             sliderInput(inputId = "years",
                         label = "Years",
                         min = 0, max = 50, 
                         value = 20, 
                         step = 1),
             selectInput(inputId = "facet", label = "Facet?", c("No", "Yes"))
           )
  ),
  hr(),
  mainPanel(h4("Timeline"),
    plotOutput("plot", width = "150%", height = "400px"),
    h4("Balance"),
    tableOutput("table")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    result = reactive({
      years = input$years
      growthrate = input$growthrate
      returnrate = input$returnRate
      annualcontribution = input$annualContribution
      intitamount = input$intitAmount
      
      no_contrib <- c()
      fixed_contrib <- c()
      growing_contrib <- c()
      
      for (i in 0:years){
        fv = future_value(intitamount, returnrate, i)
        fva = annuity(annualcontribution, returnrate, i)
        fvg = growing_annuity(annualcontribution, returnrate, growthrate, i)
        
        no_contrib[i + 1] = fv
        fixed_contrib[i + 1] = fv + fva
        growing_contrib[i + 1] = fv + fvg
      }
      
      combo = cbind("year" = 0:years, no_contrib, fixed_contrib, growing_contrib)
   
      df = data.frame(combo)
      df
})
    output$plot = renderPlot({
     result_long <- gather(result(), no_contrib:growing_contrib, key = variable, value = Balance)
      
     if (input$facet == "No"){
       g = ggplot(result_long, aes(x= year, y = Balance, col = variable))+ 
        geom_line() +geom_point() + labs(title = "Three modes of investing") + xlab("Years")+ylab("Investment") +
        scale_x_discrete(limits = 0:input$years)
     return(g)
     }else{
       ggplot(result_long, aes(x =year, y = Balance, color= variable, fill = variable))+ 
       geom_line()+ geom_point() +geom_area(stat = "identity")+
       facet_grid( ~ variable)+theme_bw()
     }
     
    }) 
    output$table = renderTable(result())
}

# Run the application 
shinyApp(ui = ui, server = server)

