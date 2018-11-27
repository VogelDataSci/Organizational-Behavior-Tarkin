#final app!

library("shinydashboard")
library("shiny")
library("igraph")
library("sna")
library("ggplot2")
library("truss")
library(data.table)
library("scales") #need this to calculate the percents!
library("DT") #need this to output the data frame!
library("ggrepel")

#Fix original maxDataSize import problem with maxRequest size
options(shiny.maxRequestSize=100*1024^5)

  #list of packages required
  list.of.packages <- c("shinydashboard", "shiny", "igraph", "sna", "ggplot2", "truss", "scales", "DT", "ggrepel")

  #checking missing packages from list
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

  #install missing ones
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

rm(list=ls())

function(input, output,session){
  z <<-0 
  w <<- 0
  l <<- 0
  charge_code <<- TRUE
  dfasd <<- NULL
  departments <- NULL
  toggle_vertex_id <<- FALSE
  single_density <<- NULL
  single_diameter <<- NULL
  single_name <<- NULL
  b_and_d_global <<- NULL
  
  projects <- NULL
  popDF <<- NULL
  numGroups <<- NULL
  numProjects <<- NULL
  nRows <<- NULL
  groupProjects <<- NULL
  nComb <<- NULL
  edgeData <<- NULL
  nodes <<- NULL
  edges <- NULL
  groups <- NULL
  emailCount <- NULL
  usage_vals <- NULL
  
  #Misc Server code for updating server-side variables based on user input (ex data headers, graph object, etc.)
  
#General Use Functions
  
  clustcoef<-function(x){ #measure clustering coefficient of network
    t<-igraph::triad.census(x)
    cltriad<-sum(t[9],t[10],t[12],t[13],t[14],t[15],t[16])
    optriad<-sum(t[4],t[5],t[6],t[7],t[8],t[11])
    clustcoef<-cltriad/(cltriad+optriad)
    clustcoef
  }
  
  X2Ygraph <- function(FromString, ToString){
    first_index = match(FromString, input_usage_vals())
    second_index = match(ToString, input_usage_vals())
    criteria <- c(data_headers()[first_index],data_headers()[second_index])
    
    #X2Ytmpadjacency <- filtered_table_by_group()[,..criteria]
    X2Yadjacency <- filtered_data_table()[,..criteria]
    X2Ygraph <- graph_from_data_frame(X2Yadjacency)
    return(X2Ygraph)
  }
  
  p2Xgraph <- function(FromString, ToString){
    first_index = match(FromString, input_usage_vals())
    second_index = match(ToString, input_usage_vals())
    criteria_1 <- c(data_headers()[first_index],data_headers()[second_index])
    criteria_2 <- c(data_headers()[second_index],data_headers()[first_index])
    p2Xadjacency <- filtered_data_table()[,..criteria_1]
    p2XadjacencyT <- filtered_data_table()[,..criteria_2]
    p2Xgraph1 <- graph_from_data_frame(p2Xadjacency)
    p2XgraphT <- graph_from_data_frame(p2XadjacencyT)
    p2graph <- compose(p2Xgraph1,p2XgraphT)
    E(p2graph)$weight <- 1
    p2graph <- simplify(p2graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
    p2graph <- igraph::delete.vertices(p2graph, which(igraph::degree(p2graph)<1))
    return(p2graph)
  }
  
  type_to_type_graph <- function(FirstType,SecondType,Mode){
    first_index = match(FirstType, input_usage_vals())
    second_index = match(SecondType, input_usage_vals())
    person_index = match("Person", input_usage_vals())
    criteria_1 <- c(data_headers()[first_index],data_headers()[person_index])
    criteria_2 <- c(data_headers()[person_index],data_headers()[second_index])
    X2Padjacency <- filtered_data_table()[,..criteria_1]
    P2Yadjacency <- filtered_data_table()[,..criteria_2]
    X2Pgraph <- graph_from_data_frame(X2Padjacency)
    P2Ygraph <- graph_from_data_frame(P2Yadjacency)
    if(Mode == 'Direct'){
      X2Ygraph <- compose(X2Pgraph,P2Ygraph)
      E(X2Ygraph)$weight <- 1
      X2Ygraph <- simplify(X2Ygraph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
      X2Ygraph <- igraph::delete.vertices(X2Ygraph, which(igraph::degree(X2Ygraph)<1))
      return(X2Ygraph)
    }
    else if(Mode == 'Through Person'){
      X2PPgraph <- compose(X2Pgraph,person_analysis_graph())
      X2Ygraph <- compose(X2PPgraph,P2Ygraph)
      E(X2Ygraph)$weight <- 1
      X2Ygraph <- simplify(X2Ygraph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
      X2Ygraph <- igraph::delete.vertices(X2Ygraph, which(igraph::degree(X2Ygraph)<1))
      return(X2Ygraph)
    }
    else{
      return(erdos.renyi.game(20, 0.4, type = "gnp"))
    }
  }
  
  jaccard_sim_function <- function(input_graph,threshold){
    
    base_graph <- input_graph
    sim <- similarity(base_graph, method = "jaccard")
    delete_idx <- which(sim <= threshold)
    #Save edge-weights
    edge_weights <- E(base_graph)$weight
    #browser()
    #mod_graph <- delete_edges(base_graph, delete_idx)
    #Turn into adjacency matrix
    tmp_adj <- as_adj(base_graph)
    tmp_adj[delete_idx] = 0
    mod_graph <- graph_from_adjacency_matrix(tmp_adj)
    E(mod_graph)$weight <- edge_weights
    mod_graph <- igraph::delete.vertices(mod_graph, which(igraph::degree(mod_graph)<1))
    return(mod_graph)}
  
  threshold_function <- function(input_graph,threshold){
    base_graph <- input_graph
    delete_idx <- which(E(base_graph)$weight <= threshold)
    #Turn into adjacency matrix
    mod_graph <- delete_edges(base_graph, delete_idx)
    # tmp_adj <- as_adj(base_graph)
    # tmp_adj[delete_idx] = 0
    # mod_graph <- graph_from_adjacency_matrix(tmp_adj)
    mod_graph <- igraph::delete.vertices(mod_graph, which(igraph::degree(mod_graph)<1))
    return(mod_graph)}
  
  between_label_plot <- function(input_graph, num_between){
    graph_bet <- igraph::betweenness(input_graph)
    sorted_dec_betw <- sort(graph_bet, decreasing = TRUE, index.return=TRUE)
    top_n_idx <- sorted_dec_betw$ix[1:num_between]
    all_idx <- c(1:vcount(input_graph))
    NA_idx <- all_idx [! all_idx %in% top_n_idx]
    vertex_attr(input_graph, "label", NA_idx) <- NA
    vertex_attr(input_graph, "label", top_n_idx) <- V(input_graph)$name[top_n_idx]
    plot(input_graph, vertex.color = 'darkblue', edge.color = 'lightblue',edge.arrow.size = 0,
         #vertex.label = NA, 
         vertex.size = 0.5, layout = layout_with_fr)
  }
  
  between_label_plot_toggle <- function(input_graph, num_between, toggle){
    if(toggle){
    graph_bet <- igraph::betweenness(input_graph)
    sorted_dec_betw <- sort(graph_bet, decreasing = TRUE, index.return=TRUE)
    top_n_idx <- sorted_dec_betw$ix[1:num_between]
    all_idx <- c(1:vcount(input_graph))
    NA_idx <- all_idx [! all_idx %in% top_n_idx]
    vertex_attr(input_graph, "label", NA_idx) <- NA
    vertex_attr(input_graph, "label", top_n_idx) <- V(input_graph)$name[top_n_idx]
    plot(input_graph, vertex.color = 'black', edge.color = 'lightblue',edge.arrow.size = 0,
         #vertex.label = NA, 
         vertex.size = 0.3, layout = layout_with_fr)
    }
    else{
      plot(input_graph, vertex.color = 'black', edge.color = 'lightblue',edge.arrow.size = 0,
           vertex.label = NA, 
           vertex.size = 0.3, layout = layout_with_fr)
    }
  }
  
  
  data_headers <- reactive({
    data_headers <- colnames(fread(input$input_file$datapath, header=TRUE,fill=TRUE))
    })
  
  input_data_table <- reactive(fread(input$input_file$datapath, header=TRUE,fill=TRUE))
  
  filtered_data_table <- reactive({
    if(!input$group_selector_input=="Unused"){
      filtered_data_table <- filtered_table_by_group()
    }
    else if(!input$project_selector_input=="Unused"){
      filtered_table_by_project()
    }
    else{
      filtered_data_table <- input_data_table()
    }
  })
  
  person_analysis_graph <- reactive({
    req(input$personPlotType)
    if(is.null(input$filteringType)){
      if (is.null(input$personPlotType)){
        person_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$personPlotType == 'PvT'){
        person_analysis_graph<-p2Xgraph("Person","Task")
      }
      else if (input$personPlotType == 'PvPr'){
        person_analysis_graph <- p2Xgraph("Person", "Project")
      }
      else{
        person_analysis_graph<-erdos.renyi.game(20, 0.4, type = "gnp")
      }
    }
    else if(input$filteringType=='JcSim'){
      if (is.null(input$personPlotType)){
        person_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$personPlotType == 'PvT'){
        person_analysis_graph <- jaccard_sim_function(p2Xgraph("Person","Task"),input$JCThreshold)
      }
      else if (input$personPlotType == 'PvPr'){
        person_analysis_graph <- jaccard_sim_function(p2Xgraph("Person","Project"),input$JCThreshold)
      }
    }
    else if(input$filteringType=='Basic_Thresh'){
      if (is.null(input$personPlotType)){
        person_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$personPlotType == 'PvT'){
        person_analysis_graph <- threshold_function(p2Xgraph("Person","Task"),input$Basic_Threshold)
      }
      else if (input$personPlotType == 'PvPr'){
        person_analysis_graph <- threshold_function(p2Xgraph("Person","Project"),input$Basic_Threshold)
      }
    }
  })
  
  group_analysis_graph <- reactive({
    req(input$groupPlotType)
    if(is.null(input$filteringType)){
      if (is.null(input$groupPlotType)){
        group_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$groupPlotType == 'GvT'){
        group_to_task<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Task"))
        group_to_task <- igraph::delete.vertices(group_to_task, which(igraph::degree(group_to_task)<1))
        # group_to_task_m <- as.matrix(as_adj(group_to_task))
        # task_to_group_m <- t(group_to_task_m)
        task_to_group<- compose(X2Ygraph("Task","Person"),X2Ygraph("Person","Group"))
        task_to_group <- igraph::delete.vertices(task_to_group, which(igraph::degree(task_to_group)<1))

        # group_to_group_m <- group_to_task_m %*% task_to_group_m
        # group_analysis_graph <- graph_from_adjacency_matrix(group_to_group_m)
        # #
        group_analysis_graph <- compose(group_to_task, task_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
      }
      else if (input$groupPlotType == 'GvPr'){
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_analysis_graph <- compose(group_to_project, project_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
      }
      else{
        group_analysis_graph<-erdos.renyi.game(20, 0.4, type = "gnp")
      }
    }
    else if(input$filteringType=='JcSim'){
      if (is.null(input$groupPlotType)){
        group_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$groupPlotType == 'GvT'){
        group_to_task<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Task"))
        group_to_task <- igraph::delete.vertices(group_to_task, which(igraph::degree(group_to_task)<1))
        
        task_to_group<- compose(X2Ygraph("Task","Person"),X2Ygraph("Person","Group"))
        task_to_group <- igraph::delete.vertices(task_to_group, which(igraph::degree(task_to_group)<1))
        group_analysis_graph <- compose(group_to_task, task_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
        group_analysis_graph <- jaccard_sim_function(group_analysis_graph,input$JCThreshold)
      }
      else if (input$groupPlotType == 'GvPr'){
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_analysis_graph <- compose(group_to_project, project_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
        group_analysis_graph <- jaccard_sim_function(group_analysis_graph,input$JCThreshold)
      }
    }
    else if(input$filteringType=='Basic_Thresh'){
      if (is.null(input$groupPlotType)){
        group_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$groupPlotType == 'GvT'){
        group_to_task<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Task"))
        group_to_task <- igraph::delete.vertices(group_to_task, which(igraph::degree(group_to_task)<1))
        
        task_to_group<- compose(X2Ygraph("Task","Person"),X2Ygraph("Person","Group"))
        task_to_group <- igraph::delete.vertices(task_to_group, which(igraph::degree(task_to_group)<1))
        
        group_analysis_graph <- compose(group_to_task, task_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
        group_analysis_graph <- threshold_function(group_analysis_graph,input$Basic_Threshold)
      }
      else if (input$groupPlotType == 'GvPr'){
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_analysis_graph <- compose(group_to_project, project_to_group)
        E(group_analysis_graph)$weight <- 1
        group_analysis_graph <- simplify(group_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        group_analysis_graph <- igraph::delete.vertices(group_analysis_graph, which(igraph::degree(group_analysis_graph)<1))
        group_analysis_graph <- threshold_function(group_analysis_graph,input$Basic_Threshold)
      }
    }
  })
  
  project_analysis_graph <- reactive({
    req(input$projectPlotType)
    if(is.null(input$filteringType)){
      if (is.null(input$projectPlotType)){
        project_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$projectPlotType == 'PrvP'){
        project_by_person<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Project"))
        E(project_by_person)$weight <- 1
        project_analysis_graph <- simplify(project_by_person, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- igraph::delete.vertices(project_analysis_graph, which(igraph::degree(project_analysis_graph)<1))
      }
      else if (input$projectPlotType == 'PrvG'){
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_analysis_graph <- compose(project_to_group, group_to_project)
        E(project_analysis_graph)$weight <- 1
        project_analysis_graph <- simplify(project_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- igraph::delete.vertices(project_analysis_graph, which(igraph::degree(project_analysis_graph)<1))
      }
      else{
        group_analysis_graph<-erdos.renyi.game(20, 0.4, type = "gnp")
      }
    }
    else if(input$filteringType=='JcSim'){
      if (is.null(input$projectPlotType)){
        project_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$projectPlotType == 'PrvP'){
        project_by_person<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Project"))
        E(project_by_person)$weight <- 1
        project_analysis_graph <- simplify(project_by_person, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- igraph::delete.vertices(project_analysis_graph, which(igraph::degree(project_analysis_graph)<1))
        project_analysis_graph <- jaccard_sim_function(project_analysis_graph,input$JCThreshold)
      }
      else if (input$projectPlotType == 'PrvG'){
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_analysis_graph <- compose(project_to_group, group_to_project)
        E(project_analysis_graph)$weight <- 1
        project_analysis_graph <- simplify(project_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- jaccard_sim_function(project_analysis_graph,input$JCThreshold)
      }
    }
    else if(input$filteringType=='Basic_Thresh'){
      if (is.null(input$projectPlotType)){
        project_analysis_graph<- erdos.renyi.game(20, 0.4, type = "gnp")
      }
      else if (input$projectPlotType == 'PrvP'){
        project_by_person<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Project"))
        E(project_by_person)$weight <- 1
        project_analysis_graph <- simplify(project_by_person, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- igraph::delete.vertices(project_analysis_graph, which(igraph::degree(project_analysis_graph)<1))
        project_analysis_graph <- threshold_function(project_analysis_graph,input$Basic_Threshold)
      }
      else if (input$projectPlotType == 'PrvG'){
        project_to_group<- compose(X2Ygraph("Project","Person"),X2Ygraph("Person","Group"))
        project_to_group <- igraph::delete.vertices(project_to_group, which(igraph::degree(project_to_group)<1))
        
        group_to_project<- compose(X2Ygraph("Group","Person"),X2Ygraph("Person","Project"))
        group_to_project <- igraph::delete.vertices(group_to_project, which(igraph::degree(group_to_project)<1))
        
        project_analysis_graph <- compose(project_to_group, group_to_project)
        E(project_analysis_graph)$weight <- 1
        project_analysis_graph <- simplify(project_analysis_graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = "sum")
        project_analysis_graph <- threshold_function(project_analysis_graph,input$Basic_Threshold)
      }
    }
  })
  
  group_to_group_graph <- reactive({type_to_type_graph("Group","Group","Through Person")})
  
  #Server code for Select Input Data Tab
  
  output$inputtable <- DT::renderDataTable({
    fread(input$input_file$datapath, header=TRUE,fill=TRUE)
  })
  output$checkboxes <- renderUI({
    #data_headers <- colnames(fread(input$input_file$datapath, header=TRUE,fill=TRUE))
    checkboxGroupInput("Data Choice", "Choose Data Classes", data_headers())
    #data_headers$thiny <- input$header_val
  })
  output$test_panels <- renderUI({
    #data_headers <- colnames(fread(input$input_file$datapath, header=TRUE,fill=TRUE))
    lapply(data_headers(), function(i) {
    selectInput(paste0('header_', i), paste0('', i),
                choices = c('Unused','Person', 'Group','Project','Task','Attribute'),
                selected='Unused')
    })
  })

input_usage_vals <- reactive({
  #index_var = 0
  #usage_vals <- list()
  #lapply(data_headers(), function(i){
  for (i in data_headers()){
    #index_var <- index_var + 1
    usage_vals[i] = input[[paste0('header_', i)]]
    }
  input_usage_vals <-usage_vals
}) 

output$selected_headers <- renderText({
  text_out = ''
  cur_input_vals <- input_usage_vals()
  #if (!is.null(cur_input_vals)){
    for(i in cur_input_vals){ #i is actually the value, code seems to be actually working!
      if (!(i == 'Unused')){ #will need to rework this
        text_out <- paste(text_out, i, sep=", ")
      }
    }
    text_out#}
})

filtered_table_by_group <- reactive({
  chosen_group <- input$group_selector_input
  if(!chosen_group == 'Unused' && input$project_selector_input=="Unused"){
    my_table <- input_data_table()
    group_header = data_headers()[match("Group", input_usage_vals())]
    person_header = data_headers()[match("Person", input_usage_vals())]
    project_header = data_headers()[match("Project", input_usage_vals())]

    persons_in_group <- unique(my_table[chosen_group, on=group_header][[person_header]])
    table_data_for_persons <- my_table[persons_in_group, on=person_header]
    all_projects <- unique(table_data_for_persons[chosen_group,on=group_header][[project_header]])
    all_data_of_interest <-my_table[all_projects,on=project_header]
    return(all_data_of_interest)
  }
  else{
    return(input_data_table())
  }
})

filtered_table_by_project <- reactive({
  chosen_project <- input$project_selector_input
  if(!chosen_project == 'Unused' && input$group_selector_input=="Unused"){
    my_table <- input_data_table()
    group_header = data_headers()[match("Group", input_usage_vals())]
    person_header = data_headers()[match("Person", input_usage_vals())]
    project_header = data_headers()[match("Project", input_usage_vals())]
    
    persons_in_project <- unique(my_table[chosen_project, on=project_header][[person_header]])
    table_data_for_persons <- my_table[persons_in_project, on=person_header]
    all_groups <- unique(table_data_for_persons[chosen_project,on=project_header][[group_header]])
    all_data_of_interest <-my_table[all_groups,on=group_header]
    return(all_data_of_interest)
  }
  else{
    return(input_data_table())
  }
})

#Server code for Network Construction Tab

output$relational_network <- renderPlot(
  if(!is.null(input$networkType)){
    # if (input$networkType=="Person"){
    # # plot(person_analysis_graph(), vertex.color = 'black', edge.color = 'lightblue',edge.arrow.size = 0,
    # #                #vertex.label = NA, 
    # #                vertex.size = 0.3, layout = layout_with_fr)
    #   between_label_plot(person_analysis_graph(),input$num_labels_display)
    # }
    # else if (input$networkType=="Group"){
    #   # plot(group_analysis_graph(), vertex.color = 'black', edge.color = 'lightblue',edge.arrow.size = 0,
    #   #      #vertex.label = NA, 
    #   #      vertex.size = 0.3, layout = layout_with_fr)
    #   between_label_plot(group_analysis_graph(),input$num_labels_display)
    # }
    # else if (input$networkType=="Project"){
    #   # plot(project_analysis_graph(), vertex.color = 'black', edge.color = 'lightblue',edge.arrow.size = 0,
    #   #      #vertex.label = NA, 
    #   #      vertex.size = 0.3, layout = layout_with_fr)
    #   between_label_plot(project_analysis_graph(),input$num_labels_display)
    # }
    # else{
    #   plot(erdos.renyi.game(10,0.5,"gnp"))
    # }
    between_label_plot(network_analysis_graph(),input$num_labels_display)
  }
  )
  
#Selectors for filtering group, project data
output$group_selector <- renderUI({
  #req(input$input_file)
  # req(input$fileType == "work")
  #df <- read.csv(input$input_file$datapath,
  #               header = input$header,
  #               sep = input$sep,
  #               quote = input$quote)
  group_index = match("Group", input_usage_vals())
  group_col <- data_headers()[group_index]
  groups <- unique(input_data_table()[,..group_col])
  group_selector <- selectInput(inputId = "group_selector_input", label="Pick a Group", choices = c("Unused",groups))
  #group_selector <- selectInput(inputId = "group_selector_input", label="Pick a Group", choices = c("Live","Die"))
})  

output$project_selector <- renderUI({
    # req(input$input_file)
    # req(input$fileType == "work")
    #df <- read.csv(input$input_file$datapath,
    #               header = input$header,
    #               sep = input$sep,
    #               quote = input$quote)
    project_index = match("Project", input_usage_vals())
    project_col <- data_headers()[project_index]
    projects <- unique(input_data_table()[,..project_col])
    project_selector <- selectInput(inputId="project_selector_input", label="Pick a Project", choices = c("Unused",projects))
    #selectInput(inputId="project_selector_input", label="Pick a Project", choices = c("Live", "Die"))
  })

output$num_groups <- renderText({
  req(input$input_file)
  req(data_headers())
  group_header = data_headers()[match("Group", input_usage_vals())]
  num_uniq_groups <- length(unique(input_data_table()[[group_header]]))
  num_groups <- paste0("Number of Unique Groups: ",num_uniq_groups)
})

output$num_tasks <- renderText({
  req(input$input_file)
  req(data_headers())
  task_header =  data_headers()[match("Task", input_usage_vals())]
  num_uniq_tasks <- length(unique(input_data_table()[[task_header]]))
  num_tasks <- paste0("Number of Unique Tasks: ",num_uniq_tasks)
})

output$num_projects <- renderText({
  req(input$input_file)
  req(data_headers())
  project_header = data_headers()[match("Project", input_usage_vals())]
  num_uniq_projects <- length(unique(input_data_table()[[project_header]]))
  num_projects <- paste0("Number of Unique Projects: ",num_uniq_projects)
})

output$num_persons <- renderText({
  req(input$input_file)
  req(data_headers())
  person_header = data_headers()[match("Person", input_usage_vals())]
  num_uniq_persons <- length(unique(input_data_table()[[person_header]]))
  num_persons <- paste0("Number of Unique Persons: ",num_uniq_persons)
})
  
  #output$person
  
  #These are for rendering outputs about the Person (Individual) Network Plot

#Get a general analysis graph

network_analysis_graph <- reactive({
  if(input$networkType=="Person"){
    network_analysis_graph <- person_analysis_graph()
  }
  else if(input$networkType=="Group"){
    network_analysis_graph <- group_analysis_graph()
  }
  else if(input$networkType=="Project"){
    network_analysis_graph <- project_analysis_graph()
  }
})
  
  output$network_analysis_plot <- renderPlot({
    req(input$fileType == "work")
    req(input$input_file)
        
    if(input$toggle_vertex_names){
      print("Let us switch")
      toggle_vertex_id <<- !toggle_vertex_id
    }
    
    if(toggle_vertex_id){
      #plot(network_analysis_graph(), edge.arrow.size = .2, vertex.label = NA, edge.color = "blue", vertex.color = "red", vertex.frame.color = "black")
      between_label_plot_toggle(network_analysis_graph(), input$num_labels_display, toggle=FALSE)
    }else{
      between_label_plot_toggle(network_analysis_graph(), input$num_labels_display, toggle=TRUE)
    }
   
  })
  
  output$density <- renderText({
    req(input$fileType == "work")
    
    if(input$statistics){
      density <- edge_density(network_analysis_graph())
      density <- paste("The Density of this Network:", format(round(edge_density(network_analysis_graph()), 3), nsmall = 3))
      
    }
  })
  
  output$median_edge_weight <- renderText({
    req(input$fileType == "work")
      edge_median <- median(E(network_analysis_graph())$weight)
      median_edge_weight <- paste("The Median Edge Weight:", format(round(edge_median, 3), nsmall = 3))
  })
  
  output$diameter <- renderText({
    req(input$fileType == "work")
    
    if(input$statistics){
      diameter <- paste("The Diameter of this Network:", diameter(network_analysis_graph()))
    }
  })
  
  output$average_distance <- renderText({
    req(input$fileType == "work")
    
    if(input$statistics){
      average_distance <- paste("The Average Path Length:", format(round(mean_distance(network_analysis_graph()), 3), nsmall = 3))
    }
  })
  
  output$average_degree <- renderText({
    req(input$fileType == "work")
    average_degree_person <- paste("The Average Node Degree:", format(round(edge_density(network_analysis_graph())*vcount(network_analysis_graph()), 3), nsmall = 3))
  })
  
  output$average_degree_analysis <- renderText({
    req(input$fileType == "work")

    if(input$statistics){
      average_degree_person <- paste("The Average Node Degree:", format(round(edge_density(network_analysis_graph())*vcount(network_analysis_graph()), 3), nsmall = 3))
    }
  })
  
  output$edge_list <- renderTable({
    req(input$fileType == "work")
    
    if(input$statistics){
      edge_list <- as_edgelist(network_analysis_graph())
    }
  })
  
  output$header <- renderText({
    req(input$fileType == "work")
    
    if(input$statistics){
      header <- "Krackhardt's Measures"
    }
  })
  
  output$hierarchy <- renderText({
    req(input$fileType == "work")
    
    if(input$statistics){
      adjacency_matrix <- get.adjacency(network_analysis_graph())
      adjacency_matrix <- as.array(adjacency_matrix)
      hierarcy <- paste("Hierarchy: ", hierarchy(adjacency_matrix, measure = "krackhardt"))
    }
  })
  
  output$efficiency <- renderText({
    if(input$statistics){
      adjacency_matrix <- get.adjacency(network_analysis_graph())
      adjacency_matrix <- as.array(adjacency_matrix)
      efficiency <- paste("        Efficiency: ", efficiency(adjacency_matrix))
    }
  })
  
  output$connectedness <- renderText({
    if(input$statistics){
      adjacency_matrix <- get.adjacency(network_analysis_graph())
      adjacency_matrix <- as.array(adjacency_matrix)
      connectedness <- paste("        Connectedness: ", connectedness(adjacency_matrix))
    }
  })
  
  output$least_upper_bound <- renderText({
    if(input$statistics){
      adjacency_matrix <- get.adjacency(network_analysis_graph())
      adjacency_matrix <- as.array(adjacency_matrix)
      efficiency <- paste("Least Upper Bound: ", lubness(adjacency_matrix))
    }
  })
  
  output$clustcoef <- renderText({
    if(input$statistics){
      clustcoef <- paste("Clustering Coef: ",clustcoef(network_analysis_graph()))
    }
  })
  
  output$characteristic_path_length <- renderText({
    if(input$person_statistics){
      characteristic_path_length <- paste("Characteristic Path Length: ",average.path.length(network_analysis_graph()))
    }
  })
  
  output$truss_plot <- renderPlot({
    if(input$truss != z){
      x <- as_edgelist(network_analysis_graph())
      #y <- make_undirected_graph(x)
      y <- graph_from_edgelist(x, directed = FALSE)
      z <<- input$truss_k
      #group_to_group_graph() <- as.undirected(network1, mode = "Collapse")
      graph <- truss(y, k = z, color_graph = FALSE)
      between_label_plot(graph, input$num_labels_display)
    }
  })
  
  output$degree_display <- renderTable({
    if(input$statistics){
      df <- data.frame(as.matrix(igraph::degree(network_analysis_graph(),normalized=TRUE)))
      df$names <- V(network_analysis_graph())$name
      
      colnames(df) <- c("Degree", "Person")
      find_top_n(df,input$num_centrality_display)
    }
  })
  
  output$betweenness_display <- renderTable({
    if(input$statistics){
      df <- data.frame(as.matrix(igraph::betweenness(network_analysis_graph(),normalized=TRUE)))
      df$names <- V(network_analysis_graph())$name
      
      colnames(df) <- c("Betweenness", "Person")
      find_top_n(df, input$num_centrality_display)
    }
  })
  
  output$between_degree_plot <- renderPlot({
    if(input$statistics){
      mygraph <- network_analysis_graph() 
      my_bet <- igraph::betweenness(mygraph, normalized=TRUE)
      my_deg <- igraph::degree(mygraph, normalized=TRUE)
      #Get Upper and Lower quartiles for the betweenness and degree centrality respectively, 
      #in terms of index
      bet_quantile <- quantile(my_bet)
      deg_quantile <- quantile(my_deg)
      low_bet <- which(my_bet <= bet_quantile[2])
      low_deg <- which(my_deg <= deg_quantile[2])
      high_bet <- which(my_bet >= bet_quantile[4])
      high_deg <- which(my_deg >= deg_quantile[4])
      low_bet_high_deg <- intersect(low_bet, high_deg)
      low_deg_high_bet <- intersect(low_deg, high_bet)
      label_set <- union(low_bet_high_deg, low_deg_high_bet)
      if(length(label_set)>0){
      bet_deg_df <- data.frame(my_deg,my_bet)
      plot(my_deg,my_bet, xlab = "Degree Centrality", ylab = "Betweenness Centrality", xlim = c(min(my_deg)-0.5,1.3*max(my_deg)+0.5))
      text(my_deg[label_set], my_bet[label_set], label=names(my_deg[label_set]),cex=0.6, pos=4, col="darkblue")}
      # aes(my_deg[label_set], my_bet[label_set], label=names(my_deg[label_set]), geom_text_repel() +
      #         geom_point(color = 'blue') +
      #         theme_classic(base_size = 16))#cex=0.6, pos=4, col="darkblue")
      
      #ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
      #geom_text_repel() +
      #  geom_point(color = 'red') +
      #  theme_classic(base_size = 16)
      
      #with(my_bet, text(my_bet, labels = names(my_bet), pos = 4))
    }
  })
  
  
  
  output$density_and_diameter <- renderTable(rownames = TRUE, {
    if(!is.null(single_name)){
      return_matrix <- matrix(ncol = 3, c(single_name, single_diameter, single_density))
      colnames(return_matrix) <- c("Name", "Diameter", "Density")
    }
  })
  

      
  observeEvent(input$download_button, {
    write_graph(network_analysis_graph(), "job-graph.gml", format = c("gml"))
  })
  
  observeEvent(input$view_button, {
    tkplot(network_analysis_graph(), 
           vertex.color = "cornflowerblue", 
           edge.color = "Black", edge.label.color = "Black", vertex.label.color = "Black", 
           vertex.size = 24, layout = layout_with_lgl)
  })

output$details_density<-renderText({
  details_density <- paste0("Network Density: ", format(round(edge_density(network_analysis_graph()), 3), nsmall = 3))
})

output$details_vcount<-renderText({
  details_vcount <- paste0("Vertex Count: ", vcount(network_analysis_graph()))
})

output$details_ecount<-renderText({
  details_ecount <- paste0("Edge Count: ", ecount(network_analysis_graph()))
})
  

  
  # output$meta_network_graph<-renderPlot({
  #   plot(erdos.renyi.game(10,0.9, "gnp")) #TODO: Add real meta-network object
  # })
}
