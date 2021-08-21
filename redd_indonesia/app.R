library(shiny)
library(dplyr)
require(maps)  #loading maps package
require(mapdata) #loading mapdata package
library(ggplot2) #ggplot2 package
library(readxl) #package for read .xlsx file
library(ggthemes) #package for ggplot2 theme
library(ggrepel) #extendig the plotting package ggplot2 for maps
library(ggthemes)
library(tidyverse)
library(scales)
library(tidyr)
library(rsconnect)

# setwd("C:\\Users\\joshu\\Documents\\git\\shiny_redd\\redd_indonesia") #set your own directory

mydata <- read_xlsx("data\\dummy.xlsx") #assign the data to "mydata"
global <- map_data("world") #World longitude and latitude data 
firedata <- read_xlsx("data\\Indonesia_fire.xlsx") 
firedata <- transform(firedata, period = as.Date(as.character(period), "%Y"))
firedata$period <- format(firedata$period, format="%Y")
firedata$wildfire_emissions_co2e <- firedata$wildfire_emissions * 1000000
freldata <- read_xlsx("data\\Indonesia_frel.xlsx") 
freldata$period <- format(freldata$period, format="%Y")
reductdata <- read_xlsx("data\\Indonesia_er.xlsx") 
reductdata$period <- format(reductdata$period, format="%Y")

all_data <- firedata %>% 
    full_join(freldata) %>% 
    full_join(reductdata) %>%
    arrange(period)

### May add a heat map of Indonesia 


# Define UI for application 
ui <- navbarPage(title = "Page", 
                 
    tabPanel("Introduction", 
                
            fluidPage(
                     
                     # Application title
                     h1("Introduction"),
                     hr(),
                     img(src = "REDD+.jpg", height = 7*10, width = 20*10),
                     br(),
                     h3("Comparing emissions due to wildfires with emissions due to deforestation, forest degradation and peatland fires."),
                     
                     img(src = "map.png", position = "right", height = 4.5*100, width = 7.29*100),
                     br(),
                     p(strong("Data sourced 2013-2016 on page A-35 of Biennial update report (BUR) (Appendix): ")), 
                     a(href="https://unfccc.int/documents/192165", "here"),
                     p(strong("Conservative historical data of 1990-2012 and 2019/2020 projections found page 44: ")),
                     a(href="https://redd.unfccc.int/files/frel_submission_by__indonesia_final.pdf", "here"),
                     
                 )
            
            ),
     tabPanel("REDD+ with Wildfires",     
              
              fluidPage(
                  
                  
                  tabsetPanel(
                      
                      tabPanel("Only Wildfires",
                               
                               # Sidebars
                               sidebarLayout(position = "right",
                                             
                                             
                                             sidebarPanel(
                                                 
                                                 
                                                 helpText("Example: Span of 0.65 means each of the local regressions 
                                                used to produce that curve incorporates 65% of the total data points"),
                                                 
                                                 sliderInput("fire", "Span:",
                                                             min = 0.2, max = 1,
                                                             value = 0.5, step = 0.1),
                                                 
                                                 selectInput("fire1",
                                                             label = "Confidence interval: ",
                                                             choices = list("90%" = 0.9,
                                                                            "95%" = 0.95,
                                                                            "99%" = 0.99),
                                                             selected = 0.95), 
                                                 
                                                 selectInput("fire2",
                                                             label = "formula: ",
                                                             choices = list("y ~ x" = 'auto',
                                                                            "y ~ log(x)" = 'lm'),
                                                             selected = 'auto'),
                                                 
                                             ),
                                             
                                             # Generating distribution and Img
                                             mainPanel(plotOutput("fire", 
                                                                  click = "plot_click",
                                                                  dblclick = "plot_dblclick"),
                                                       verbatimTextOutput("info")
                                                       
                                             ),
                                             
                               )),
                      
                      tabPanel("REDD+ with wildfires", plotOutput("simple", 
                                                   click = "plot_click2",
                                                   dblclick = "plot_dblclick2"),
                               verbatimTextOutput("info2"))
                      )
         
                     
                )
     
        ),
            
                 tabPanel("Comparing Emissions with Reference Levels", 
                          plotOutput("referencelvl", 
                                      click = "plot_click3",
                                      dblclick = "plot_dblclick3"),
                                      verbatimTextOutput("info3")
                          ),
    
                 tabPanel("Total Forest Emissions", 
                          plotOutput("totalem", 
                          click = "plot_click4",
                          dblclick = "plot_dblclick4"),
                          verbatimTextOutput("info4")))
    
    


# Define server logic
server <- function(input, output) {
    
    
    output$fire <- renderPlot({
        
          p1 <-ggplot(firedata, aes(x=as.double(period), y=wildfire_emissions)) +
          geom_point(size=3, shape=23, color="purple3", fill="purple3") +
          geom_smooth(method = input$fire2, span = as.double(input$fire), level=as.double(input$fire1)) +
          theme_minimal() +
          theme(axis.title.x = element_text(vjust=1), axis.title.y.left = element_text(vjust=+10)) +
          labs(title = "Simple Wildfire Emissions plot",
               subtitle = "(2000-2020)",
               caption = "Data from CAMS GFAS and FREL submissions (check data description for more details)",
               tag = "Figure 1",
               x = "Period", y = "Carbon Emissions (Megatonnes)") 
        
        p1
        
    })
    
    output$simple <- renderPlot({
        
        # Data preparation

        some_data <- subset(all_data, period > 2000)
        df <- some_data %>%
            select(period, wildfire_emissions_co2e, total_annual_emissions, frel_reference) %>%
            gather(key = "Variables", value = "Values", -period)

        df$Variables[df$Variables == "wildfire_emissions_co2e"] <- "Wildfire Emissions"
        df$Variables[df$Variables == "total_annual_emissions"] <- "Total Forest Emissions"
        df$Variables[df$Variables == "frel_reference"] <- "Forest Reference Emission Levels"
        df$Values <- df$Values/1000000
        
        firedata1 <- firedata[df[["period"]] >= "2012", ]
        firedata2 <- firedata[df[["period"]] < "2012", ]
        firedata1 <- subset(firedata, period >= 2012)
        firedata2 <- subset(firedata, period < 2012)

        # Visualization
        g <- ggplot(df, aes(x = as.double(period), y = Values)) + 
          geom_line(size = 1, aes(color = Variables)) + 
          scale_color_manual(values = c("goldenrod1", "coral1", "black", "black")) +
          labs(title = "Simple REDD+ plot",
               subtitle = "(2000-2020)",
               caption = "Data from CAMS GFAS and FREL submissions (check data description for more details)",
               tag = "Figure 2",
               x = "Period", y = "Carbon Emissions/Reference Levels (Megatonnes)") +
          geom_point(size = 3, aes(color=Variables)) +
          theme_minimal() +
          theme(legend.position="top") +
          theme(axis.title.x = element_text(vjust=1), axis.title.y.left = element_text(vjust=+10)) +
          scale_x_continuous(breaks= pretty_breaks())
        
        g <- g + geom_point(data=firedata1, aes(x=as.double(period), y=wildfire_emissions, fill = "Fires pre-REDD+"), size=3, shape=23) + 
          scale_shape_identity() + geom_smooth(data = firedata1, aes(x=as.double(period), y=wildfire_emissions), method = lm, span = 0.5, level=0.95) + 
          geom_point(data=firedata2, aes(x=as.double(period), y=wildfire_emissions, fill = "Fires during REDD+"), size=3, shape=23) + 
          geom_smooth(data = firedata2, aes(x=as.double(period), y=wildfire_emissions), method = lm, span = 0.5, level=0.95)
        
        g
        
    })
    
    output$referencelvl <- renderPlot({
      
      # Data preparation
      
      df <- all_data %>%
        select(period, deforestation_reference, forestdegradation_reference, peatdecomposition_reference, deforestation_actual, forestdegradation_actual, peatdecomposition_actual) %>%
        gather(key = "Variables", value = "Values", -period)
      
      df$Variables[df$Variables == "wildfire_emissions_co2e"] <- "Wildfire Emissions"
      df$Variables[df$Variables == "deforestation_reference"] <- "Deforestation Reference"
      df$Variables[df$Variables == "forestdegradation_reference"] <- "Degradation Reference"
      df$Variables[df$Variables == "peatdecomposition_reference"] <- " Peat decomposition Reference"
      df$Variables[df$Variables == "deforestation_actual"] <- "Deforestation"
      df$Variables[df$Variables == "forestdegradation_actual"] <- "Forest degradation"
      df$Variables[df$Variables == "peatdecomposition_actual"] <- "Forest peat decomposition"
      df$Values <- df$Values/1000000
      df <- df[!(df[["period"]] >= "2012" & df[["Variables"]] == "Deforestation"), ]
      df <- df[!(df[["period"]] >= "2012" & df[["Variables"]] == "Forest degradation"), ]
      df <- df[!(df[["period"]] >= "2012" & df[["Variables"]] == "Forest peat decomposition"), ]
      df <- df[df[["period"]] >= "1990" & df[["period"]] <= "2020", ]
      
      firedata1 <- firedata[df[["period"]] >= "2012", ]
      firedata2 <- firedata[df[["period"]] < "2012", ]
      firedata1 <- subset(firedata, period >= 2012)
      firedata2 <- subset(firedata, period < 2012)
        
      # Visualization
      g <- ggplot(df, aes(x = as.double(period), y = Values)) +
        geom_line(size = 1, aes(color = Variables)) +
        scale_color_manual(values = c("darkred", "red", "blue3", "hotpink", "darkorchid1", "cornflowerblue", "red", "green")) +
        labs(title = "Comparing Emissions With Reference Levels In Indonesia",
             subtitle = "(1990-2020)",
             caption = "Data from CAMS GFAS and FREL submissions (check data description for more details)",
             tag = "Figure 3",
             x = "Period", y = "Carbon Emissions/Reference Levels (Megatonnes)") +
        geom_point(size = 4, aes(color=Variables)) +
        theme_minimal() +
        theme(legend.position="top") +
        theme(axis.title.x = element_text(vjust=1), axis.title.y.left = element_text(vjust=+10)) +
        scale_x_continuous(breaks= pretty_breaks())
      
      
      g <- g + geom_point(data=firedata1, aes(x=as.double(period), y=wildfire_emissions, fill = "Fires pre-REDD+"), size=4, shape=23) + 
        scale_shape_identity() + geom_smooth(data = firedata1, aes(x=as.double(period), y=wildfire_emissions), method = lm, span = 0.5, level=0.95) + 
        geom_point(data=firedata2, aes(x=as.double(period), y=wildfire_emissions, fill = "Fires during REDD+"), size=4, shape=23) + 
        geom_smooth(data = firedata2, aes(x=as.double(period), y=wildfire_emissions), method = lm, span = 0.5, level=0.95)
      
      g
      
    })
    
    output$totalem <- renderPlot({
      
      # Data preparation
      
      df <- all_data %>%
        select(period, wildfire_emissions_co2e, frel_reference, deforestation_actual,
               forestdegradation_actual, peatdecomposition_actual, total_annual_emissions) %>%
        gather(key = "Variables", value = "Values", -period)
      
      df$Variables[df$Variables == "wildfire_emissions_co2e"] <- "Wildfires"
      df$Variables[df$Variables == "frel_reference"] <- "FREL"
      df$Variables[df$Variables == "deforestation_actual"] <- "Deforestation"
      df$Variables[df$Variables == "forestdegradation_actual"] <- "Forest degradation"
      df$Variables[df$Variables == "peatdecomposition_actual"] <- "Forest peat decomposition"
      df$Variables[df$Variables == "total_annual_emissions"] <- "Total emissions"
      df$Values <- df$Values/1000000
      
      # Visualization
      g <- ggplot(df, aes(x = as.double(period), y = Values)) + 
        geom_line(size=1, aes(color = Variables)) + 
        scale_color_manual(values = c("red", "steelblue", "darkgreen", "goldenrod1", "coral1", "purple3")) +
        labs(title = "Total Forest Carbon Emissions In Indonesia",
             subtitle = "(1990-2020)",
             caption = "Data from CAMS GFAS and FREL submissions (check data description for more details)",
             tag = "Figure 4",
             x = "Period", y = "Carbon Emissions Equivalent (Megatonnes)") +
        geom_point(size=4, aes(color=Variables)) +
        theme_minimal() +
        theme(legend.position="top") +
        theme(axis.title.x = element_text(vjust=1), axis.title.y.left = element_text(vjust=+10)) +
        scale_x_continuous(breaks= pretty_breaks())
      
      g
      
    })
    
    
    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Year ", format(round(e$x), format="%Y") , " versus " , round(e$y, 1), " Megatonnes of Carbon Emissions (MtCO2e) \n")
        }
        
        paste0(
            "click: ", xy_str(input$plot_click),
            "dblclick: ", xy_str(input$plot_dblclick)
        )
    })
    
    output$info2 <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("Year ", format(round(e$x), format="%Y") , " versus " , round(e$y, 1), " Megatonnes of Carbon Emissions (MtCO2e) \n")
        }
        
        paste0(
            "click: ", xy_str(input$plot_click2),
            "dblclick: ", xy_str(input$plot_dblclick2)
        )
    })
    
    output$info3 <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("Year ", format(round(e$x), format="%Y") , " versus " , round(e$y, 1), " Megatonnes of Carbon Emissions (MtCO2e) \n")
      }
      
      paste0(
        "click: ", xy_str(input$plot_click3),
        "dblclick: ", xy_str(input$plot_dblclick3)
      )
    })
    
    output$info4 <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("Year ", format(round(e$x), format="%Y") , " versus " , round(e$y, 1), " Megatonnes of Carbon Emissions (MtCO2e) \n")
      }
      
      paste0(
        "click: ", xy_str(input$plot_click4),
        "dblclick: ", xy_str(input$plot_dblclick4)
      )
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
