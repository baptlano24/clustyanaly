#' davies_bouldin function
#'
#'function which calculates the Davies Bouldin coefficient where we can decide the type of distance
#'
#'
#'@references  \url{https://en.wikipedia.org/wiki/Davies-Bouldin_index}
#'
#'
#'
#'@param X data_frame or matrix of features must be numeric
#'@param clust vector of result of the cluster, numeric or string
#'@param distance str of type of distance see {\link[stats]{dist}} \url{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist}
#'@return davies_bouldin index
#'@import stats
#' @author Baptiste Lanoue
#' @export
davies_bouldin<-function(X,clust,distance="euclidean")
  {
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  df_mean <-X[FALSE,]
  vect_dist <- c()
  vect_max <- c()
  vect_db <- c()
  for (clu in un){
    clus_in <- df[df$clust==clu,]
    clus_out <- df[df$clust!=clu,]
    group_x <- clus_in[1:(length(clus_in)-1)]
    #calculate centroid
    mean_clu <- apply(group_x,2,function(x){mean(x, na.rm=TRUE)})
    #calculate distance between centroid and each point of cluster
    dist_clu <- apply(group_x,1,function(x) { dist(rbind(x,mean_clu),method = distance) }  )
    #calculate distance mean of centroid
    mean_dist <- mean(dist_clu)
    #stock the result
    df_mean[nrow(df_mean)+1,] <- mean_clu
    vect_dist <-append(vect_dist,mean_dist)
  }
  # 2 loop for calculate every possibility, choose the max stock and then reset
  for (i in 1:length(vect_dist))
  {
    for(j in 1:nrow(df_mean)){
      if(i!=j){
        db <-(vect_dist[i] + vect_dist[j])/dist(rbind(df_mean[i,],df_mean[j,]),method = distance)
        vect_db <- append(vect_db,db)
      }
    }
    max_db <- max(vect_db)
    #reset
    vect_db <- c()
    #stock the max value
    vect_max <- append(vect_max,max_db)
  }
  #calculate the mean
  db_max <- mean(vect_max)
  return(list(davies_bouldin=db_max))
}
