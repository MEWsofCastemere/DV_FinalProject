# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(gridExtra)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1}) 
  
  df1 <- eventReactive(input$clicks, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from ADAMDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) %>% group_by(., YEAR, STATE) %>% mutate(KPI = cumsum(DAMAGE_KPI)) %>% mutate(MAX = max(KPI)) %>% select(YEAR, STATE, MAX) %>% distinct() })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      scale_fill_gradient2(low="white", mid = "red", high= "darkred", midpoint = 100000) +
      labs(title=input$title) +
      labs(x=paste("Year"), y=paste("State")) +
      layer(data=df1(), 
            mapping=aes(x= as.character(YEAR), y=STATE, fill = MAX), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(), 
            position=position_identity()
      )
    
    plot
  })
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  dfs <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from ADAMDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) %>% select(YEAR, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, INJURIES_DIRECT, INJURIES_INDIRECT, STATE) %>% mutate(TOTAL_INJURIES = INJURIES_DIRECT + INJURIES_INDIRECT)})
  #dfs1 <- select(dfs, BEGIN_YEARMONTH, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, BEGIN_DAY) %>% filter(DEATHS_DIRECT != 0)
  #df2 <- eventReactive(input$clicks2, {dfs1})
  
  output$distPlot2 <- renderPlot(height=1000, width=1000, {
    DDPY <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~YEAR, ncol = 1) +
      labs(title='StormEvents Barchart\ndeaths_direct, avg(deaths_direct), ') +
      labs(x=paste("Year"), y=paste("Deaths Direct")) +
      layer(data=dfs(), 
            mapping=aes(x=YEAR, y=(DEATHS_DIRECT)), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(color="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfs(), 
            mapping=aes(yintercept = 33.9), 
            geom="hline",
            geom_params=list(colour="red")
      )
    DDPY
    IDPY <- ggplot() + 
      coord_cartesian() + 
      #scale_color_gradient2(mid = ("Green"), high="Red") +
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~YEAR, ncol = 1) +
      labs(title='StormEvents Barchart\ndeaths_indirect, avg(deaths_indirect), ') +
      labs(x=paste("Year"), y=paste("Deaths Indirect")) +
      layer(data=dfs(), 
            mapping=aes(x=YEAR, y=(DEATHS_INDIRECT)), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(color="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfs(), 
            mapping=aes(yintercept = 1.33), 
            geom="hline",
            geom_params=list(colour="red")
      )
    IDPY
    DCPY <- ggplot() + 
      coord_cartesian() + 
      #scale_color_gradient2(mid = ("Green"), high="Red") +
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~YEAR, ncol = 1) +
      labs(title='StormEvents Barchart\nDAMAGE_CROPS, avg(DAMAGE_CROPS), ') +
      labs(x=paste("Year"), y=paste("Damage Crops")) +
      layer(data=dfs(), 
            mapping=aes(x=YEAR, y=(DAMAGE_CROPS)), 
            stat="identity", 
            stat_params=list(), 
            geom=("bar"),
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfs(), 
            mapping=aes(yintercept = 266), 
            geom="hline",
            geom_params=list(colour="red")
      )
    DCPY
    DPPY <- ggplot() + 
      coord_cartesian() + 
      #scale_color_gradient2(mid = ("Green"), high="Red") +
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~YEAR, ncol = 1) +
      labs(title='StormEvents Barchart\nDAMAGE_PROPERTY, avg(DAMAGE_PROPERTY), ') +
      labs(x=paste("Year"), y=paste("DAMAGE PROPERTY")) +
      layer(data=dfs(), 
            mapping=aes(x=YEAR, y=(DAMAGE_PROPERTY)), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfs(), 
            mapping=aes(yintercept = 2000), 
            geom="hline",
            geom_params=list(colour="red")
      )
    DPPY
    plot1 <- grid.arrange(DDPY, IDPY, DCPY, DPPY)
    plot1
  })
  
  # Begin code for Third Tab:
  
  df4 <- eventReactive(input$clicks4, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from ADAMDATA where DAMAGE_CROPS is not null and DAMAGE_PROPERTY is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) %>% select(MAGNITUDE, DAMAGE_CROPS, DAMAGE_PROPERTY, END, BEGIN, YEAR, DEATHS_DIRECT, DEATHS_INDIRECT) %>% filter(DAMAGE_PROPERTY > 0) %>% mutate (TIME_DIFFERENCE = (abs(END - BEGIN))) %>% mutate (TOTAL_DEATHS = (DEATHS_DIRECT + DEATHS_INDIRECT)) %>% filter(TIME_DIFFERENCE > 0, TOTAL_DEATHS > 0)})
  
  
  output$distPlot4 <- renderPlot({
    plot4 <- ggplot() + 
      coord_cartesian() + 
      scale_color_gradient2(mid = ("Yellow"), high="Red") +
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Property Damage versus Time of Event') +
      labs(x="Total Time of Disaster", y=paste("Property Damage")) +
      layer(data=df4(), 
            mapping=aes(as.numeric(TIME_DIFFERENCE), y=as.numeric(as.character(DAMAGE_PROPERTY)), color = (MAGNITUDE)), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    plot4
  })
})
