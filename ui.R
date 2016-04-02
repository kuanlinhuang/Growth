#Define UI for application 

#Plotting Growth Curves"
shinyUI(navbarPage("Growth-Curves",theme="Slate.css",
                 # Sidebar 
             tabsetPanel(type = "pills",
                               
          tabPanel("Upload Data",tags$hr(),        
                   column(8,
          sidebarPanel(
          fileInput("file1", h4('Choose txt File'),
                                                    accept=c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv')),
                                          
                                          tags$hr(),
                                          checkboxInput('header', h5('Header'), TRUE),
                                          
                                          radioButtons('sep', h5('Separator'),
                                                       c(Comma=',',
                                                         Semicolon=';',
                                                         Tab='\t'),
                                                       '\t'),
                                          radioButtons('quote', h5('Quote'),
                                                       c(None='',
                                                         'Double Quote'='"',
                                                         'Single Quote'="'"),
                                                       ''),
                                          radioButtons('dec', h5('Dec'),
                                                       c(Comma=',',
                                                         Point='.'
                                                       ),'.')
                                          ,width=3)
                                        ,
                                        br(),
          
          selectInput("obs", label = h5("Number of observations to view"), 
                      choices = list("5" = 5, "10" = 10,
                                     "15" = 15,"20" = 20,"All"=0), selected = 1)),
          column(4,tableOutput("contents"))),
          
          tabPanel("Models",
                   tags$hr(),        
                     column(4, wellPanel(
                      uiOutput("Dep"),
                      uiOutput("VarI"),
                      uiOutput("Interaction"),
                     br()
                     #uiOutput("Todos"),
                     
                     
                   )), column(8,wellPanel(mainPanel(tableOutput("regTab")),tableOutput("Anov")))),
          navbarMenu("Plots",                       
                     tabPanel("Growes-curves",                   
                              column(6,plotOutput("GrPlot")),
                              column(4, 
                                     wellPanel( br(),
                                                uiOutput("Yvar"),
                                                uiOutput("Xvar"),
                                                uiOutput("Factor"),
                                                sliderInput("cexLab", "cex-Main", min = 0.1, max = 2, value = 0.8),
                                                downloadButton('DownloadGr', 'Download')
                                     ))
                              
                              
                     ),
                     
                     tabPanel("Density-Plots",                   
                              column(6,plotOutput("DensityPlot")),
                              column(4, 
                                     
                                     wellPanel( br(),
                                                uiOutput("VarInt"),
                                                uiOutput("FactoD"),
                                                downloadButton('DensityPlot1', 'Download')
                                             
                                     ))
                              
                              
                     )
          
          
          
          
                               
                   )))
)
