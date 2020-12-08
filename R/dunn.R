#' Dunn function
#'
#' function which calculates the Dunn coefficient with different distance inter neigbour cluster: centroid, min and max
#' we can also decide the type of distance
#'
#'@references  \url{https://en.wikipedia.org/wiki/Dunn_index}
#'
#'@param X data_frame or matrix of features must be numeric
#'@param clust vector of result of the cluster, numeric or string
#'@param distance str of type of distance see {\link[stats]{dist}} \url{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist}
#'@author Baptiste Lanoue
#'
#'@return A list with different dunn value.
#' \itemize{
#'   \item dunn_mean - Dunn coefficient with distance inter cluster by centroid.
#'   \item dunn_min - Dunn coefficient with distance inter cluster by the min.
#'   \item dunn_max - Dunn coefficient with distance inter cluster by the max.
#' }
#'@import stats
#'@export
dunn <-
  function(X,clust,distance="euclidean"){
  return(list(dunn_mean = dunn_mean(X,clust,distance), dunn_min = dunn_min(X,clust,distance), dunn_max = dunn_max(X,clust,distance)))
  }

dunn_mean<-function(X,clust,distance="euclidean"){
  #dunn evaluation by the ratio of the min of the distance inter cluster with the centroid of each cluster and the max intra cluster distance
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  df_mean <-X[FALSE,]
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #calculate the centroid for each cluster
    mean_clu <- apply(group_x,2,  function(x) { mean(x, na.rm=TRUE) })
    #calculate the max distance intra for each cluster
    max_dist <- max(dist(group_x,method=distance))
    df_mean[nrow(df_mean)+1,] <- mean_clu
    vect_dist <-append(vect_dist,max_dist)
  }
  #calculate the ratio of the distance for each centroid cluster's by the max intra distance of all cluster
  dunn_val <- min(dist(df_mean,method=distance))/max(vect_dist)
}

dunn_min<-function(X,clust,distance="euclidean"){
  #dunn evaluation by the ratio of the min of the distance inter cluster between closer point for each cluster and the max intra cluster distance
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  vect_min <-c()
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #apply function which return distance with the neighbor cluster for each cluster by the min distance
    mean_clu <- apply(group_x,1,function(x) distance_min_intra(x1=x,X=group_x,Y=clus_out,distance=distance))
    #calculate the max distance intra for each cluster
    max_dist <- max(dist(group_x,method=distance))
    vect_min <- append(vect_min,mean_clu)
    vect_dist <-append(vect_dist,max_dist)
  }
  #ratio of the min distance between neighbor cluster and the max distance of all cluster
  dunn_val <- min(vect_min)/max(vect_dist)
}

dunn_max<-function(X,clust,distance="euclidean"){
  #dunn evaluation by the ratio of the min of the distance inter cluster between farther points for each cluster and the max intra cluster distance
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  vect_min <-c()
  vect_dist <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #apply function which return distance with the neighbor cluster for each cluster by the max distance
    mean_clu <- apply(group_x,1,function(x) distance_max_intra(x1=x,X=group_x,Y=clus_out,distance=distance))
    max_dist <- max(dist(group_x))
    vect_min <- append(vect_min,mean_clu)
    vect_dist <-append(vect_dist,max_dist)
  }
  dunn_val <- min(vect_min)/max(vect_dist)
}


distance_min_intra <- function(x1,X,Y,distance){
  #function which calculate the distance min of a point and the closer cluster
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  #function which calculate the distance min of a point and the points of the other clusters
  clust_list <- sapply(test,function(x) distance_min_voi(x1,x,distance))
  #select the min distance between the point and all other cluster
  b <- min(unlist(clust_list))
}

distance_min_voi <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1),method=distance)))
  #return the min distance between x and the other points of the cluster
  b_min <- min(b_vec)
}

distance_max_intra <- function(x1,X,Y,distance){
  #function which calculate the distance max of a point and the points of the other clusters
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  clust_list <- sapply(test,function(x) distance_max_voi(x1,x,distance))
  #choose the closer point of max distance
  b <- min(unlist(clust_list))
}

distance_max_voi <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1),method=distance)))
  #calculate the distance and chose the max
  b_min <- max(b_vec)
}




