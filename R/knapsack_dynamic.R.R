#'Dynamic programming algorithm for the knapsack problem
#'
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details An algorithm that can solve the knapsack problem exact by iterating over all possible values of w all possbile values with good accuracy , and also gives the maximum value for the knapsack
#'@author Aqeel Ahmed, Marhawi Tewolde
#'@examples
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#'knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@seealso
#'\code{\link{Dynamic Programming Knapsack}}
#'@export
knapsack_dynamic<- function(X,W)
{
  stopifnot(is.data.frame(X),is.numeric(W))
  
  a1<-as.vector(X[,1])
  a<-c(0,a1)
  b1<-as.vector(X[,2])
  b<-c(0,b1)
  n<-length(a1)
  aa<-c(1:n)
  rr<-c()
  rmat<-matrix(NA,nrow =n+1,ncol = W+1)
  elements<-matrix(0,nrow = n+1,ncol = W+1)
  rmat[,1]<-0
  rmat[1,]<-0
  j<-2
  while(j<=W+1)
  { 
    i<-2
    while(i<=n+1)
    {
      if(j < a[i])
      {
        rmat[i,j]<-rmat[i-1,j]
        
      }
      
      else
      {
        rmat[i,j]<-max(rmat[i-1,j-a[i]]+b[i],rmat[i-1,j])
      }
      
      i=i+1
      
    }
    
    j=j+1  
    
  }
  
  for(j in 2:W+1)
  {
    
    for(i in 2:n+1)
      if(rmat[i,j]!=0 && rmat[i-1,j]!=rmat[i,j])
      {
        elements[i,j]<- 1
      }
    else
    {
      elements[i,j]<-0
    }
  }
  counter1<-n+1
  counter2<-W+1
  while(counter1>0)
  {
    if(elements[counter1,counter2]==1)
    {
      rr<-c(rr,(counter1)-1)
      counter1<-counter1-1
      counter2<-counter2-a1[counter1]
    }
    else
    {
      rr<-rr
      counter1<-counter1-1
      
    }
  }
  result<-rmat[n+1,W+1]
  rownames(rmat) = c(1:length(a))
  return(list(value=round(result),elements=sort(rr)))
}