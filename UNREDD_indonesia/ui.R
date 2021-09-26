#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Page", 
                   
                   tabPanel("Introduction", 
                            
                            fluidPage(
                                
                                # Application title
                                h1("Introduction"),
                                hr(),
                                img(src = "REDD+.jpg", height = 7*10, width = 20*10),
                                br(),
                                h3("This application is dedicated to comparing emissions due to wildfires with emissions due to deforestation, forest degradation and peatland fires."),
                                
                                img(src = "map.png", position = "right", height = 4.5*100, width = 7.29*100),
                                br(),
                                p(strong("Data sourced 2012-2017 on page A-35 of Biennial update report (BUR) (Appendix): ")), 
                                a(href="https://unfccc.int/documents/192165", "here"),
                                p(strong("Conservative historical data of 1990-2012 and 2019/2020 projections found page 44: ")),
                                a(href="https://redd.unfccc.int/files/frel_submission_by__indonesia_final.pdf", "here"),
                                p(strong("Emissions due to wildfires collected via WEB API and processed: ")),
                                a(href="https://apps.ecmwf.int/datasets/data/cams-gfas/", "here"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                
                            )
                            
                   ),
                   tabPanel("REDD+ with Wildfires",     
                            
                            fluidPage(
                                
                                
                                tabsetPanel(
                                    
                                    tabPanel("Wildfire Trend",
                                             
                                             # Sidebars
                                             sidebarLayout(position = "right",
                                                           
                                                           
                                                           sidebarPanel(
                                                               
                                                               
                                                               helpText("Example: Span of 0.65 means each of the local regressions 
                                                used to produce that curve incorporates 65% of the total data points."),
                                                               
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
                            verbatimTextOutput("info4"))))
