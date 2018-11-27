#final app!

library("shinydashboard")
library("igraph")
library("DT")

  dashboardPage(
     dashboardHeader(title = "TARKIN"
    #                 dropdownMenu(type = "notifications",
    #                              notificationItem(
    #                                text = "5 new collaboration opportunities today",
    #                                icon("users")
    #                              )
    #                 )             
     ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                 menuSubItem("Return to Dashboard", tabName = "dashboard"),
                 
                 input_file <- fileInput("input_file", "Choose Input File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 #checkboxInput("header", "Header", TRUE),
                 #We'll move header files to the window where the user specifies columns and such
                 radioButtons("fileType", "File Type",
                              choices = c("Work Analysis" = "work", "Email Analysis" = "email"),
                              selected = "work")
                 
                 # Input: Select separator ----
                 # radioButtons("sep", "Separator",
                 #              choices = c(Comma = ",",
                 #                          Semicolon = ";",
                 #                          Tab = "\t"),
                 #              selected = ","),
                 # 
                 # # Input: Select quotes ----
                 # radioButtons("quote", "Quote",
                 #              choices = c(None = "",
                 #                          "Double Quote" = '"',
                 #                          "Single Quote" = "'"),
                 #              selected = '"'),
                 # 
                 # # Horizontal line ----
                 # tags$hr(),
                 # 
                 # # Input: Select number of rows to display ----
                 # radioButtons("disp", "Display",
                 #              choices = c(Head = "head",
                 #                          All = "all"),
                 #              selected = "head"),
                 # 
                 # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                 #                   label = "Search...")         
                 
        ),
        menuItem("Select Data from Inputfile", tabName="selectData", icon = icon("th")),
        menuItem("Network Construction", tabName = "net_con", icon = icon("th")),
        menuItem("Network Analysis", tabName = "network_analysis", icon = icon("th"))#,
        #menuItem("Email Data", tabName = "email", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # tabItem(tabName = "dashboard",
        #         fluidRow(
        #           # Output: Data file ----
        #           box(title = "Data", width = 12, dataTableOutput("contents"))
        #         )
        # ),
        tabItem(tabName = "selectData",
                # Boxes need to be put in a row (or column)
                 box(
                   title = "Input Data Display", width = NULL, status = "primary",
                   div(style = 'overflow-x: scroll', DT::dataTableOutput('inputtable'))
                 )
                ,
                box(
                  title = "Data Select", width = NULL, status = "primary",
                     div(style = 'overflow-x: scroll', #uiOutput('checkboxes'),
                         uiOutput('test_panels')
                         # create some select inputs
                         #conditionalPanel(
                        #   condition = "input == true",
                         #  selectInput("header_val", "Method",
                          #             list("lm", "glm", "gam", "loess", "rlm"))
                         )
                ),
                box(
                  title = "Output of Data select", width = NULL, status = "primary",
                  div(style = 'overflow-x: scroll', textOutput('selected_headers')
                      # create some select inputs
                      #conditionalPanel(
                      #   condition = "input == true",
                      #  selectInput("header_val", "Method",
                      #             list("lm", "glm", "gam", "loess", "rlm"))
                  )
                )
                # box(
                #   title = "Data Select", width = NULL, status = "primary",
                #   div(style = 'overflow-x: scroll', checkboxGroupInput("1", "Checkbox Input", textOutput('data_headers')
                #     , selected = NULL, inline = FALSE, width = NULL))
                # )
                #h2("Input Data Display"),
                #DT::dataTableOutput("inputtable")
        ),
        tabItem(tabName = "net_con",
                # Boxes need to be put in a row (or column)
                fluidRow(
                  column(width = 5,
                         box(title = "Pick a relational network:" , width = NULL, status = "primary", 
                             solidHeader = TRUE, radioButtons("networkType", "Types", 
                                                              choices = c("Person x Person Analysis" = "Person",
                                                                          "Group x Group Analysis" = "Group",
                                                                          "Project x Project Analysis" = "Project"),
                                                              selected = character(0))),
                         conditionalPanel(
                           condition = "input.networkType == 'Person'",
                           box(title = "Pick a relational network:" , width = NULL, status = "primary", 
                               solidHeader = TRUE, 
                           radioButtons("personPlotType", "Types", 
                                        choices = c("People connected by Tasks" = "PvT",
                                                    "People connected by Projects" = "PvPr",
                                                    "People connected by Email" = "PvE"),
                                        selected = character(0)))
                         ),
                         conditionalPanel(
                           condition = "input.networkType == 'Group'",
                           box(title = "Pick a relational network:" , width = NULL, status = "primary", 
                               solidHeader = TRUE,
                           radioButtons("groupPlotType", "Types", 
                                        choices = c("Groups connected by Tasks" = "GvT",
                                                    "Groups connected by Projects" = "GvPr",
                                                    "Groups connected by Email" = "GvE"),
                                        selected = character(0)))
                           ),
                         conditionalPanel(
                           condition = "input.networkType == 'Project'",
                           box(title = "Pick a relational network:" , width = NULL, status = "primary", 
                               solidHeader = TRUE, 
                           radioButtons("projectPlotType", "Types", 
                                        choices = c("Projects connected by People" = "PrvP",
                                                    "Projects connected by Groups" = "PrvG",
                                                    "Projects connected by Email" = "PrvE"),
                                        selected = character(0)))
                         ),
                         box(title = "Select Filtering Technique", status = "primary", width = NULL, solidHeader = TRUE, 
                             radioButtons("filteringType", "Types", choices = c("Jacard Similarity" = "JcSim",
                                                      "Thresholding" = "Basic_Thresh"), 
                                                      selected = character(0)
                                          ),
                             conditionalPanel(
                               condition = "input.filteringType == 'JcSim'",
                               numericInput('JCThreshold', "Jacard Similarity Threshold", 0.0, min = 0.0, max = 1.0)
                             ),
                             conditionalPanel(
                               condition = "input.filteringType == 'Basic_Thresh'",
                               numericInput('Basic_Threshold', "Number of Shared Connections Threshold Value", 0, min = 0, max = 500)
                             )),
                         box(
                           title= "Select Group for Group-centric Analysis", status = "primary", width = NULL,
                           solidHeader = TRUE, uiOutput("group_selector")
                         ),
                         box(
                           title= "Select Project for Project-centric Analysis", status = "primary", width = NULL,
                           solidHeader = TRUE, uiOutput("project_selector")
                         ),
                         box(
                           title = "Number of Unique Groups, Tasks, Projects, Persons in Organization",
                           status = "primary", width = NULL,solidHeader = TRUE,
                           textOutput("num_groups"),
                           textOutput("num_tasks"),
                           textOutput("num_projects"),
                           textOutput("num_persons")
                         )
                  ),
                  column(width = 7,
                         fluidRow(
                         box(title = "Relational Network Plot", status = "primary", width = NULL, height = 500,
                             solidHeader = TRUE, plotOutput("relational_network", width = "100%")),
                         box(
                           title = "Select Number of Top Betweenness Centrality Labels to Display", status = "primary",
                           width = NULL, solidHeader = TRUE, numericInput(inputId = "num_labels_display", min = 1, max = 50, 
                           value = 5, label = "Enter Number of Top Centrality Scores to Display")
                         ),
                         box(title = "Network Details", status = "primary", width = NULL, solidHeader = TRUE,
                             textOutput("details_density"),
                             textOutput("average_degree"),
                             textOutput("median_edge_weight"),
                             textOutput("details_vcount"),
                             textOutput("details_ecount")
                             ),
                         box(title = "Save/View Network", status = "primary", width = NULL, solidHeader = TRUE,
                           actionButton("download_button", "download plot"),
                           actionButton("view_button", "view interactive")
                         ))
                  )
                )
        ),
        tabItem(tabName = "network_analysis",
                #tabsetPanel(
                  #tabPanel("Person x Person Analysis", 
                           fluidRow(
                             h3("")
                           ),
                           fluidRow(
                             box(
                               title = "Network Analysis",
                               #title = paste0(networkType," Based Network"),  
                               plotOutput("network_analysis_plot"),
                               actionButton(inputId = "statistics",label = "Get Statistics", 
                                            height = 125),
                               actionButton(inputId = "toggle_vertex_names", label = "Toggle Vertex IDs", 
                                            height = 125),
                               textOutput("density"),
                               textOutput("diameter"),
                               textOutput("average_distance"),
                               textOutput("average_degree_analysis"),
                               textOutput("clustcoef"),
                               tags$b(textOutput("header")),
                               textOutput("hierarchy"),
                               textOutput("efficiency"),
                               textOutput("connectedness"),
                               textOutput("least_upper_bound")
                             ),
                             box(
                               title = "Betweenness/Degree Plot",
                               plotOutput("between_degree_plot")
                             )
                           ),
                           # fluidRow(
                           #   box(
                           #     uiOutput("person_selector")
                           #   )
                           # ),
                           fluidRow(
                             box(
                               h3("Centrality Measures")
                             ),
                             box(
                               numericInput(inputId = "num_centrality_display", min = 1, max = 50, 
                                            value = 10, label = "Enter Number of Top Centrality Scores to Display")
                             )
                           ),
                           fluidRow(
                             box(
                               title = "Degree",
                               tableOutput("degree_display")
                             ),
                             box(
                               title = "Betweenness",
                               tableOutput("betweenness_display")
                             )
                           ),
                           fluidRow(
                             box(
                               h3("Truss Graphs")
                             )
                           ),
                           fluidRow(
                             box(
                               title = "Truss Graph For Person",
                               plotOutput("truss_plot", height = 250),
                               actionButton(inputId = "truss", label = "Get Truss Plot", height = 125),
                               numericInput(inputId = "truss_k", min = 3, max = 50, 
                                            value = 3, label = "Enter K Value")
                             )
                           )
                           )

            )
                
        )
  ) #https://rstudio.github.io/shinydashboard/structure.html
  