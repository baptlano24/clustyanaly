#'Variance_Index
#'
#' function which calculates variance internal criteria : Calinski_Harabasz, Ball_hall, Hartigan and Xu"
#'
#'@param X data_frame of features must be numeric
#'@param clust vector of result of the cluster, numeric or string
#'
#'@references  \url{https://slideplayer.com/slide/6982424/}
#'
#'@author Baptiste Lanoue
#'
#'
#'@return A list with different internal criteria value.
#' \itemize{
#'   \item calinski_harabasz - calinski_harabasz coefficient
#'   \item ball_hall - Ball_hall coefficient.
#'   \item Hartigan -  Hartigan coefficient.
#'   \item Xu -  Xu coefficient.
#' }
#' @export
variance_index <- function(X,clust){
  #function which return all internal criteria in a list
  return(list(calinski_harabasz = calinski_harabasz(X,clust),ball_hall=ball_hall(X,clust),Hartigan=hartigan(X,clust),Xu = Xu(X,clust)))
}
calinski_harabasz<-function(X,clust){
  #function which calculate calinski_harabasz index
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  #calculate mean of the X
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  #split df in function of the cluster
  test <- split(df, f = df$clust)
  #apply the function Variance Within cluster
  list_wk <- sapply(test,function(x) wk_var(x,m))
  #apply the function Variance between cluster
  list <- sapply(test,function(x) B_var(x,m))
   B <- sum(unlist(list))
   wk <- sum(unlist(list_wk))
  DEM <- (N-numb_clust)*B
  NUM <- (numb_clust - 1)*wk
  SCH <- (DEM)/(NUM)
}

ball_hall<-function(X,clust){
  #function which calculate ball_hall index
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  #split df in function of the cluster
  test <- split(df, f = df$clust)
  #apply the function Mean Variance Within cluster
  list_wk <- sapply(test,function(x) wss(x,m))
  wk <- sum(unlist(list_wk))
  b_h <- wk/(numb_clust)
}

hartigan<-function(X,clust){
  #function which calculate hartigan index
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  test <- split(df, f = df$clust)
  #apply the function Variance Within cluster
  list_wk <- sapply(test,function(x) wk_var(x,m))
  #apply the function Variance between cluster
  list <- sapply(test,function(x) B_var(x,m))
  B <- sum(unlist(list))
  wk <- sum(unlist(list_wk))
  har <- log(B/wk)
}
Xu <- function(X,clust){
  #function which calculate Xu index
  un <- unique(clust)
  numb_clust <- length(un)
  df <- cbind(X, clust)
  N <- nrow(X)
  D <- ncol(X)
  #calculate mean of the X
  m <- apply(X,2,function(x){mean(x, na.rm=TRUE)})
  test <- split(df, f = df$clust)
  list_wk <- sapply(test,function(x) wss(x,m))
  wk <- sum(unlist(list_wk))
  xu <- D * log(sqrt(wk/(D*(N^2)))) + log(numb_clust)


}

B_var <- function(x,m){
  #the function Variance between cluster
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  mk <- nrow(y)*(mk-m)^2
}

wk_var <- function(x,m){
  #calculate Variance Within cluster
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  wk <- apply(y,1,function(y){(y-mk)^2})
  w <- sum(wk)

}

wss <- function(x,m){
  #calculate Variance Within cluster
  y <- x[1:(length(x)-1)]
  mk <- apply(y,2,function(x){mean(x, na.rm=TRUE)})
  wk <- apply(y,1,function(y){(y-mk)^2})
  w <- sum(wk)/nrow(y)

}

