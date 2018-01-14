filenames <- list.files("C:\\Users\\Galia\\Desktop\\kaggle\\project\\csv_data", full.names = T)

datalist <- lapply(filenames, function(x){
  x0 <- read.csv(file = x, header=TRUE, row.names=1, check.names=FALSE)
  return(x0)
})

adj_list <- lapply(datalist, function(x){
  m <- as.matrix(x)
  return(m)
})
# adj_list[2]

g_list <- lapply(adj_list, function(x){
  g <- graph.adjacency(x, mode = "undirected", weighted = NULL)
  return(g)
})
# g_list[1]

walktrap_list <- lapply(g_list, function(x){
  wc <- walktrap.community(x)
  return(modularity(wc))
})
# walktrap_list[3]

fastgreedy_list <- lapply(g_list, function(x){
  wc <- fastgreedy.community(x)
  return(modularity(wc))
})
# fastgreedy_list

ebetween_list <- lapply(g_list, function(x){
  wc <- edge.betweenness.community(x)
  return(modularity(wc))
})
# ebetween_list

spinglass_list <- lapply(g_list, function(x){
  wc <-  spinglass.community(x)
  return(modularity(wc))
}) 
# spinglass_list 

leigenvector_list <- lapply(g_list, function(x){
  wc <- leading.eigenvector.community(x)
  return(modularity(wc))
})
# leigenvector_list

lpropagation_list <- lapply(g_list, function(x){
  wc <- label.propagation.community(x)
  return(modularity(wc))
})
# lpropagation_list

# install.packages("Rfast")
library(Rfast)
# install.packages("DescTools")
library(DescTools)

detect <- function(){
  A <- rbind(unlist(walktrap_list), unlist(fastgreedy_list), unlist(ebetween_list), unlist(spinglass_list), unlist(leigenvector_list), unlist(lpropagation_list))
  methods_names <- c("walktrap.community", "fastgreedy.community", "edge.betweenness.community", "spinglass.community", "leading.eigenvector.community", "label.propagation.community")
  best_method <- colMaxs(A)
  M <- Mode(best_method)
  res <- list()
  if (length(M) > 1) {
    no <- round(mean(best_method))
    res[1] <- methods_names[no]
    res[2] <- A[no, ]
    return(res)
  }
  else{
    no <- M
    res[1] <- methods_names[no]
    res[2] <- data.frame(A[M, ])
    return(res)
  }
}
detect()

