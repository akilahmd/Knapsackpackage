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
  i=2
  optimum_value = 0     
  selected_items = c()
  weights<-c()
  values<-c()
  while(i<=nrow(X))
  {
  w<-as.data.frame(combn(X[,1], i))
  v<-as.data.frame(combn(X[,2], i))
  sumw<-colSums(w)
  sumv<-colSums(v)
  weights<-which(sumw<=W)
  if(length(weights) != 0){ 
  values<-sumv[weights]
  optimum_value<-max(values)
  temp<-which((values)==optimum_value)
  maxValWghtIdx<-weights[temp]
  maxValWght<-w[, maxValWghtIdx]
  j<-1
  while (j<=i){
    selected_items[j]<-which(X[,1]==maxValWght[j])
    j=j+1
  }
  }
  i=i+1
 
  }
 
  return(list(value=round(optimum_value),elements=selected_items))
}
#trial to github
#second change