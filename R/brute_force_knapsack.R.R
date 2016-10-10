#'Brute force algorithm for the knapsack problem
#'
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm gives all possbile values with good accuracy , and also gives the maximum value for the knapsack
#'@author Aqeel Ahmed, Marhawi Tewolde
#'@examples
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#'brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@seealso
#'\code{\link{Brute Force Knapsack}}
#'@export

brute_force_knapsack <- function(X, W){
  stopifnot(is.data.frame(X),is.numeric(W))
  a1<-as.vector(X[,1])
  b1<-as.vector(X[,2])
  n = length(a1)
  aa<-c(1:n)
  optimum_value = 0     
  selected_items = c()     
  v<-combn(b1,2)
  w<-combn(a1,2)
  aaw<-t(combn(aa,2))
  v<-t(v)
  w<-t(w)
  sumw<-rowSums(w)
  sumv<-rowSums(v)
  r<-which(sumw<=W)
  values<-sumv[r]
  optimum_value<-max(values)
  bb<-which(sumv==optimum_value)
  bb<-aaw[bb,]
  value<-round(optimum_value)
  
  return(list(value=value, elements=bb))
  
}