# Paquets a instal.lar/carregar
pckgs<-c("shiny","shinyjs","shinythemes","shinydashboard", "ggthemes",
         "tidyverse","XML","wordcloud","tm","slam","diptest","DT","gplots")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

source("dashboard_functions.R")

shinyUI(fluidPage(
  navbarPage("",
             theme = shinytheme("cerulean"),
             tabPanel("Data Input and Problem Definition",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            tags$b(h4("Problem/Class Description")),
                            textInput("classDescription",NULL,value="(not specified)"),
                            
                            tags$b(h4("Data Input")),
                            checkboxInput("checkbox", "Analyse per User (unselect to analyse per filename)", FALSE),
                            fileInput('logsImport', 'Log Files', multiple=TRUE),
                            fileInput('obsMilestonesImport', 'Observational Milestones (optional)',
                                      accept=c('text/plain', '.txt')),
                            fileInput('evMilestonesImport', 'Evaluation Milestones (optional)',
                                      accept=c('text/plain', '.txt'))
                          ),
                          mainPanel(
                            verbatimTextOutput("studentData",placeholder=TRUE),
                            verbatimTextOutput("milestonesData",placeholder=TRUE),
                            verbatimTextOutput("evmilestonesData",placeholder=TRUE)
                          ),
                          position = "left"
                        )
                      )
             ),
             tabPanel("Global Results",
                      navlistPanel(
                        tabPanel("Time-on-Task Distribution",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("toT_dist"),
                                     htmlOutput("toT_text1")
                                   )
                                 )
                        ),
                        tabPanel("Number-of-Actions Distribution", 
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("nActions"),
                                     htmlOutput("nActions_text1")
                                   )
                                 )
                        ),
                        tabPanel("Number-of-Actions vs Time-on-Task",
                                 fluidPage(
                                   verticalLayout(
                                     uiOutput("plotuinAct"),
                                     verbatimTextOutput("plot_pointsnAct")
                                   )
                                 )
                        ),     
                        tabPanel("Action time by student",
                                 fluidPage(
                                   verticalLayout(
                                     uiOutput("plotui"),
                                     verbatimTextOutput("plot_points")
                                   )
                                 )
                        ),
                        tabPanel("Functions Wordcloud",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("wcloud")
                                   )
                                 )
                        ),
                        widths = c(2, 10)
                      )
             ),
             tabPanel("Observation Milestones",
                      navlistPanel(
                        tabPanel("Average per Milestone",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("proportionbars")
                                   )
                                 )
                        ),
                        tabPanel("Heatmap",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("heatmap")
                                   )
                                 )
                        ),
                        widths = c(2, 10)
                      )
             ),
             tabPanel("Evaluation Milestones",
                      navlistPanel(
                        tabPanel("Average per Milestone",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("evproportionbars")
                                   )
                                 )
                        ),
                        tabPanel("Heatmap",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("evheatmap")
                                   )
                                 )
                        ),
                        tabPanel("Grade Distribution",
                                 fluidPage(
                                   verticalLayout(
                                     plotOutput("gradedistr"),
                                     htmlOutput("gradedistr_text1")
                                   )
                                 )
                        ),
                        tabPanel("Actions-Grade-Time Scatterplot",
                                 fluidPage(
                                   verticalLayout(
                                     uiOutput("plotuinActionsVStoTVSgrade"),
                                     verbatimTextOutput("plot_pointsnActionsVStoTVSgrade")
                                   )
                                 )
                        ),
                        widths = c(2, 10)
                      )
             ),
             tabPanel("Student-specific Results",
                      htmlOutput("selectStudent"),
                      navlistPanel(
                        tabPanel("Student Summary",
                                 fluidPage(
                                   verticalLayout(
                                     htmlOutput("studentanalysis_text")
                                     #plotOutput("heatmapStudent")
                                   )
                                 )
                        ),
                        tabPanel("Work History",
                                 fluidPage(
                                   verticalLayout(
                                     dataTableOutput("studentCmdHistory")
                                   )
                                 )
                        ),
                        widths = c(2, 10)
                        
                      )
             )
  )
)
)
