#'Greedy algorithm for the knapsack problem
#'
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm gives the approximate answer to our problem,it is able to show at least 50% of the true maximum values. And the value given by this algorithm has low computational time.
#'@author Aqeel Ahmed, Marhawi Tewolde
#'@examples
#'greedy_knapsack(knapsack_objects[1:800], 3500)
#'greedy_knapsack(knapsack_objects[1:1200], 2000)
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'@seealso
#'\code{\link{Greedy Algorithm}}
#'@export


greedy_knapsack <- function(x, W){
  
  
  stopifnot(is.data.frame(x),is.numeric(W))
  
  
  x$rows_idx <- row(x)
  x <- x[x$w < W,]
  x$vw_ratio <- x$v/x$w
  dsc_ordr <- order(x$vw_ratio, decreasing = TRUE)
  x <- x[dsc_ordr,]
  
  rslt <- list(value = 0)
  curr_weight <- 0
  
  i <- 1
  
  repeat{
    if(curr_weight <= W){
      curr_weight <- curr_weight + x$w[i]
      
      rslt$value <- rslt$value + x$v[i]
      rslt$elements[i] <- x$row[i]
      
      i <- i + 1
      if(i>nrow(x) | curr_weight+x$w[i]>W){
        break()
      }
    } 
  }
  
  rslt$value <- round(rslt$value,0)
  return(rslt)
}
