library(igraph)
filenames <- list.files("C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\R_training", full.names = T)

datalist <- lapply(filenames, function(x){
  x0 <- read.csv(file = x, header=TRUE, row.names=1, check.names=FALSE)
  return(x0)
})

adj_list <- lapply(datalist, as.matrix)
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

mult_louv_list <- lapply(g_list, function(x){
  wc <- cluster_louvain(x)
  return(modularity(wc))
})

infomap_list <- lapply(g_list, function(x){
  wc <- cluster_infomap(x)
  return(modularity(wc))
})
# lpropagation_list

# install.packages("Rfast")
library(Rfast)
# install.packages("DescTools")
library(DescTools)

#detect which method gave higher modularity value
detect <- function(){
  A <- rbind(unlist(walktrap_list), unlist(fastgreedy_list), unlist(ebetween_list), unlist(spinglass_list), unlist(leigenvector_list), unlist(lpropagation_list), unlist(mult_louv_list), unlist(infomap_list))
  methods_names <- c("walktrap.community", "fastgreedy.community", "edge.betweenness.community", "spinglass.community", "leading.eigenvector.community", "label.propagation.community", "louvain.community", "infomap.community")
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

#spinglass has the best results
#save clusters into final group
final_spinglass <- lapply(g_list, function(x){
  wc <-  spinglass.community(x)
  return(wc)
})
final_group <- lapply(final_spinglass, membership)
#final louvain
final_lgroup <- lapply(g_list, cluster_louvain)
final_l_gr <- lapply(final_lgroup, membership)

#save fast greedy and leading eogenvectors clustering
final_fgreedy <- lapply(g_list, fastgreedy.community)
final_fgreedy_gr <- lapply(final_fgreedy, membership)

final_leigen <- lapply(g_list, leading.eigenvector.community)
final_leigen_gr <- lapply(final_leigen, membership)


#save clusters as csv files

save_clusters <- function(var, method){
  name0 <- "C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\training_results\\"
  name00 <- paste(name0, method, sep='')
  dir.create(name00)
  l <- length(var)
  for(i in 1:l){
    df <- as.data.frame(do.call(cbind, var[i]))
    name1 <- paste(name00, "\\", sep = '')
    name2 <- paste(name1, rownames(df)[1], sep = '')
    name3 <- paste(name2, ".csv", sep = '')
    write.csv(df, name3)
  }
}

#save spinglass
save_clusters(final_group, "spinglass")
save_clusters(final_l_gr, "louvain")
save_clusters(final_fgreedy_gr, "fast greedy")
save_clusters(final_leigen_gr, "leading eigenv")

#plots making
A <- rbind(unlist(walktrap_list), unlist(fastgreedy_list), unlist(ebetween_list), unlist(spinglass_list), unlist(leigenvector_list), unlist(lpropagation_list), unlist(mult_louv_list), unlist(infomap_list))
TA <- t(A)
colnames(TA) <- c("walktrap.community", "fastgreedy.community", "edge.betweenness.community", "spinglass.community", "leading.eigenvector.community", "label.propagation.community", "louvain.community", "infomap.community")
numers <- matrix(1:dim(TA)[1], ncol = dim(TA)[2], nrow = dim(TA)[1])

library(graphics)

windows()
matplot(numers, TA, type = "b", col = 1:dim(TA)[2], lty = 1, lwd = seq(2, 0.1, length=dim(TA)[2]), xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("walktrap", "fastgreedy", "edge.betweenness", "spinglass", "leading.eigenvector", "label.propagation", "louvian", "info map"), col = 1:dim(TA)[2], lty = 1, lwd = seq(2, 0.1, length=dim(TA)[2]), bty = "n", cex = 0.90, seg.len = 0.95)
axis(1, at=c(1, dim(TA)[1]), labels = c(1, dim(TA)[1]))

#walktrap&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(1, 4)], type = "b", col = c("black", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("walktrap", "spinglass"), col = c("black", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#fast.greedy&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(2, 4)], type = "b", col = c("blue", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("fast greedy", "spinglass"), col = c("blue", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#edge.betweenness&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(3, 4)], type = "b", col = c("darkgreen", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("Edge-betweenness", "spinglass"), col = c("darkgreen", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#leading.eigenvector&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(5, 4)], type = "b", col = c("purple4", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("leading eigenvector", "spinglass"), col = c("purple4", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#propagating labels&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(6, 4)], type = "b", col = c("gray35", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("propagating labels", "spinglass"), col = c("gray35", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#louvain&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(7, 4)], type = "b", col = c("dark green", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("louvain", "spinglass"), col = c("dark green", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#infomap&spinglass
windows()
matplot(numers[, c(1, 2)], TA[, c(8, 4)], type = "b", col = c("black", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(2, dim(TA)[1]+0.5), ylim = c(0, 0.95), xaxp = c(1, dim(TA)[1], dim(TA)[1] - 1), pch = "o", cex = 0.5)
legend(32, 0.92, c("infomap", "spinglass"), col = c("black", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)


#TEST

test_names <- list.files("C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\R_test", full.names = T)

test_datalist <- lapply(test_names, function(x){
  x0 <- read.csv(file = x, header=TRUE, row.names=1, check.names=FALSE)
  return(x0)
})

test_adj_list <- lapply(test_datalist, as.matrix)
# adj_list[2]

test_g_list <- lapply(test_adj_list, function(x){
  g <- graph.adjacency(x, mode = "undirected", weighted = NULL)
  return(g)
})
# g_list[1]

test_fastgreedy_list <- lapply(test_g_list, function(x){
  wc <- fastgreedy.community(x)
  return(modularity(wc))
})

test_spinglass_list <- lapply(test_g_list, function(x){
  wc <-  spinglass.community(x)
  return(modularity(wc))
}) 

test_leigenvector_list <- lapply(test_g_list, function(x){
  wc <- leading.eigenvector.community(x)
  return(modularity(wc))
})

test_mult_louv_list <- lapply(test_g_list, function(x){
  wc <- cluster_louvain(x)
  return(modularity(wc))
})


detect <- function(){
  A <- rbind(unlist(test_fastgreedy_list), unlist(test_spinglass_list), unlist(test_leigenvector_list), unlist(test_mult_louv_list))
  methods_names <- c("fastgreedy.community", "spinglass.community", "leading.eigenvector.community", "louvain.community")
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

#spinglass has the best results
#save clusters into final group
test_final_spinglass <- lapply(test_g_list, spinglass.community)
test_final_group <- lapply(test_final_spinglass, membership)
#final louvain
test_final_lgroup <- lapply(test_g_list, cluster_louvain)
test_final_l_gr <- lapply(test_final_lgroup, membership)

#save fast greedy and leading eogenvectors clustering
test_final_fgreedy <- lapply(test_g_list, fastgreedy.community)
test_final_fgreedy_gr <- lapply(test_final_fgreedy, membership)

test_final_leigen <- lapply(test_g_list, leading.eigenvector.community)
test_final_leigen_gr <- lapply(test_final_leigen, membership)

#save clusters as csv files

save_clusters <- function(var, method){
  name0 <- "C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\test_results\\"
  name00 <- paste(name0, method, sep='')
  dir.create(name00)
  l <- length(var)
  for(i in 1:l){
    df <- as.data.frame(do.call(cbind, var[i]))
    name1 <- paste(name00, "\\", sep = '')
    name2 <- paste(name1, rownames(df)[1], sep = '')
    name3 <- paste(name2, ".csv", sep = '')
    write.csv(df, name3)
  }
}

#save spinglass
save_clusters(test_final_group, "spinglass")
save_clusters(test_final_l_gr, "louvain")
save_clusters(test_final_fgreedy_gr, "fast greedy")
save_clusters(test_final_leigen_gr, "leading eigenv")

#plots making
test_A <- rbind(unlist(test_fastgreedy_list), unlist(test_spinglass_list), unlist(test_leigenvector_list), unlist(test_mult_louv_list))
test_TA <- t(test_A)
colnames(test_TA) <- c("fastgreedy.community", "spinglass.community", "leading.eigenvector.community", "louvain.community")
tnrow = dim(test_TA)[1]
tncol = dim(test_TA)[2]
test_numers <- matrix(1:tnrow, ncol = tncol, nrow = tnrow)



#faswindows()
matplot(test_numers, test_TA, type = "b", col = 1:tncol, lty = 1, lwd = seq(0, 0.1, length=tncol), xlab = "egonets", ylab = "modularity", xlim = c(1, tnrow+0.5), ylim = c(0, 0.95), xaxp = c(1, tnrow, tnrow - 1), pch = "o", cex = 0.5)
legend(17, 0.92, c("fastgreedy", "spinglass", "leading.eigenvector", "louvian"), col = 1:tncol, lty = 1, lwd = seq(2, 0.1, length=tncol), bty = "n", cex = 0.90, seg.len = 0.95)
axis(1, at=c(1, tnrow), labels = c(1, tnrow))

# fast.greedy&spinglass
windows()
matplot(test_numers[, c(1, 2)], test_TA[, c(1, 2)], type = "b", col = c("blue", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(1, tnrow+0.5), ylim = c(0, 0.95), xaxp = c(1, tnrow, tnrow - 1), pch = "o", cex = 0.5)
legend(17, 0.92, c("fast greedy", "spinglass"), col = c("blue", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#leading.eigenvector&spinglass
windows()
matplot(test_numers[, c(3, 2)], test_TA[, c(3, 2)], type = "b", col = c("purple4", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(1, tnrow+0.5), ylim = c(0, 0.95), xaxp = c(1, tnrow, tnrow - 1), pch = "o", cex = 0.5)
legend(17, 0.92, c("leading eigenvector", "spinglass"), col = c("purple4", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#louvain&spinglass
windows()
matplot(test_numers[, c(4, 2)], test_TA[, c(4, 2)], type = "b", col = c("dark green", "red"), lty = 1, xlab = "egonets", ylab = "modularity", xlim = c(1, tnrow+0.5), ylim = c(0, 0.95), xaxp = c(1, tnrow, tnrow - 1), pch = "o", cex = 0.5)
legend(17, 0.92, c("louvain", "spinglass"), col = c("dark green", "red"), lty = 1, bty = "n", cex = 0.90, seg.len = 0.95)

#plot graph
dat = read.csv('C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\R_training\\graph-11364.csv', header=TRUE, row.names=1, check.names=FALSE)
m = as.matrix(dat)
# head(m)


g = graph.adjacency(m, mode = "undirected", weighted = NULL)

#plotting
E(g)$color <- rgb(0, 0, 0, alpha = .2)
m_ego <- names(which.max(degree(g)))
V(g)[V(g) != m_ego]$color = 'blue'
V(g)[m_ego]$color = 'red'

wc_fastgreedy <- fastgreedy.community(g)
modularity(wc_fastgreedy)

wc_spinglass <- spinglass.community(g)
modularity(wc_spinglass)

wc_leading <- leading.eigenvector.community(g)
modularity(wc_leading)

wc_louvain <- cluster_louvain(g)
modularity(wc_louvain)

windows()
plot(g, vertex.label = NA, vertex.size = 15, vertex.color = membership(wc_fastgreedy),
     layout = layout.fruchterman.reingold)

windows()
plot(g, vertex.label = NA, vertex.size = 15, vertex.color = membership(wc_leading),
     layout = layout.fruchterman.reingold)

windows()
plot(g, vertex.label = NA, vertex.size = 15, vertex.color = membership(wc_louvain),
     layout = layout.fruchterman.reingold)

windows()
plot(g, vertex.label = NA, vertex.size = 15, vertex.color = membership(wc_spinglass),
     layout = layout.fruchterman.reingold)

true_dat = read.csv('C:\\Users\\Galia\\Dropbox\\DataRoot University\\kaggle\\NEW\\R_training\\graph-11364.csv', header=TRUE, row.names=1, check.names=FALSE)