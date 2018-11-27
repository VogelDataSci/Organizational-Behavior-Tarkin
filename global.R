

#Function to return the top 10 (at the most) nodes in terms of degree
find_top_n <- function(df,n){
  
  #Make a new data.frame that will be return, it will hold the values from highest to lowest
  holder_df <- data.frame()
  
  #Keep track of a temporary pair (degree, vertexID) 
  #and keep track of the highest degree for easy comparison
  highest_vertex <- NULL
  highest_degree <- 0
  index <- 1
  
  while(nrow(df) > 0 && !nrow(holder_df) == n){
    for(vertex_index in c(1:nrow(df))){
      
      temporary_vertex <- df[vertex_index,]
      
      temporary_degree <- (df[vertex_index,][[1]])
      
      if(temporary_degree > highest_degree){
        highest_degree <- temporary_degree
        highest_vertex <- temporary_vertex
        index <- vertex_index
      }
    }
    holder_df <- rbind(holder_df, highest_vertex)
    highest_vertex <- NULL
    highest_degree <- 0
    df <- df[-index,]
    index <- 1
  }
  
  return(holder_df)
}



