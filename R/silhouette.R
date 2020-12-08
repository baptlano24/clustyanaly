#' Silhouette function
#'
#'function which calculates the Silhouette coefficient where we can decide the type of distance
#'@references  \url{https://fr.wikipedia.org/wiki/Silhouette_(clustering)}
#'
#'@param X data_frame or matrix of features must be numeric
#'@param clust vector of result of the cluster, numeric or string
#'@param distance str of type of distance see {\link[stats]{dist}} \url{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist}
#'@author Baptiste Lanoue
#'@return A list with Silhouette coefficient.
#' \itemize{
#'   \item s_coeff - Silhouette Coefficient of the result
#'   \item vect_point - Silhouette Coefficient for each observations of the data
#'   \item vect_clus -  Silhouette Coefficient for each cluster
#' }
#' @import stats
#' @export
silhouette<-function(X,clust,distance="euclidean"){
  #function which calculates the Silhouette coefficient where we can decide the type of distance
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  vect_s <- c()
  s_clust<- c()
  s_sum <- 0
  i <- 1
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    Ik <- nrow(clus_in)
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #silhouette coefficient per point for each cluster
    s <- apply(group_x,1,function(x) distance_moyenne_a(x1=x,X=group_x,Y=clus_out,distance=distance))
    vect_s <- append(vect_s,s)
    s_sum <- sum(s)/Ik
    s_clust <- append(s_clust,s_sum)
    names(s_clust)[i]<-clu
    i <- i +1
  }
  dtf_s <- data.frame(point = names(vect_s), s = vect_s)
  s_total <- sum(s_clust)/numb_clust
  return(list(s_coeff = s_total, vect_point = vect_s, vect_clust =s_clust))
}

distance_moyenne_a <- function(x1,X,Y,distance){
  #average distance of a point in its cluster
  test <- split(Y, f = Y$clust)
  Ik <- nrow(X)
  #distance of a point from the other points of its cluster
  a_vec <- apply(X, 1, function(x) (dist(rbind(x,x1),method=distance)))
  #average distance of a point in its cluster
  a <- sum(a_vec)/(Ik-1)
  #calculate the average distance from the point to the other points of each neighboring cluster
  clust_list <- sapply(test,function(x) distance_moyenne_b(x1,x,distance))
  #choisir le cluster voisin (distance moyenne) le plus proche
  b <- min(unlist(clust_list))
  #appliquer la formule de silhouette pour un point
  s <- (b-a)/max(b,a)
}

distance_moyenne_b <- function(x1,X,distance){
  Y <- X[1:(length(X)-1)]
  Ik <- nrow(Y)
  b_vec <- apply(Y, 1, function(x) (dist(rbind(x,x1),method=distance)))
  b <- sum(b_vec)/Ik
}
