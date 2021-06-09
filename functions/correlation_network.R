#----------------------
# CORRELATION_NETWORK :
#----------------------
# This function creates a plot of the correlation between assets and returns the selected options.
#
# INPUT : asset returns of stocks
# OUTPUT : logical vector of the main stocks
#
# Please install the igraphic package for this to work.
#
# The correlation network that is plotted highlights the main stock of each cluster. The selection of the main stock is done by choosing the
# one that has the minimum distance to all other stocks in the cluster.

library(stats)
library(igraph)

correlation_network <- function(returns){
  # Asset returns & correlation matrix
  corr_returns <-  cor(returns, use="pairwise.complete.obs")
  # Computing the distance :
  distance <- sqrt(2*(1-corr_returns))
  
  # Minimum spanning tree with Igraph :
  #------------------------------------
  MST_distance <- graph.adjacency(distance, weighted=TRUE, mode="undirected")
  MST_distance <- minimum.spanning.tree(MST_distance)
  
  # Algo to spot clusters : cluster_fast_greedy or cluster_infomap
  clusters <- cluster_fast_greedy(MST_distance)
  
  # Detecting which asset best represents the each cluster
  groups <- clusters$membership
  unique_groups <- unique(groups)
  stock_index <- 1:nrow(distance)
  group_main_stocks <- NULL
  for (ii in 1:length(unique_groups)) {
    group_index <- groups==unique_groups[ii] # Stocks that are in the same group/cluster
    group_stocks_index <- stock_index[group_index] # Index of the stocks that are in the group/cluster
    group_distance <- distance[group_index,group_index] # Distance among each stock in the group/cluster
    
    # Selecting the stock with minimum distance to the others
    if (length(group_stocks_index)>1) {
      group_distance <- rowSums(group_distance, na.rm = FALSE, dims = 1)
      index_min <- which.min(group_distance)
    }
    else {
      index_min<-1
    }
    group_main_stocks[ii] <- group_stocks_index[index_min]
  }
  # Plotting the Igraph :
  size_vertex <- 0 * stock_index + 1
  size_vertex[group_main_stocks] <- 6
  
  #pdf('rplot.jpg')
  plot(clusters,MST_distance, vertex.color = clusters$membership,edge.label = NA,vertex.label = NA,vertex.size=size_vertex,edge.arrow.size=0.01)
  #dev.off()
  
  main_stocks <- stock_index * FALSE
  main_stocks[group_main_stocks] <- TRUE
  
  return(as.logical(main_stocks))
}