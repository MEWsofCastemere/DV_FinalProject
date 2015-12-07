#ui.R 

library(shiny)

navbarPage(
  title = "Storm Event Shiny App",
  tabPanel(title = "Crosstab",
           sidebarPanel(
             sliderInput("Low", "Low Min:", 
                         min = 0, max = 25000,  value = 1000),
             sliderInput("Med", "Moderate Min:", 
                         min = 25001, max = 100000,  value = 30000),
             sliderInput("High", "Intense Min:", 
                         min = 100001, max = 200000,  value = 100001),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Damage KPI by Year and Type"),
             actionButton(inputId = "clicks", 
                          label = "Generate Plot")
           ),
           
           mainPanel(plotOutput("distPlot1")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             actionButton(inputId = "clicks2",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot2")
           )
  ),
  
  tabPanel(title = "Scatter Plot",
           sidebarPanel(
             actionButton(inputId = "clicks4", label = "Click me")
           ),
           mainPanel(plotOutput("distPlot4")
           )
  )
)

