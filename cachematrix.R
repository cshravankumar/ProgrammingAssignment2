#Matrix inversion is usually a costly computation 
#and there may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly.
#This sequence of code would do just that using solve() 
#function and caching methods implemented in R

#makeCacheMatrix is a list(set,get,setmatrix,getmatrix) that sets the cached value of a matrix, gets the cached value of matrix,
#sets the cached inverse of matrix and gets the cached inverse of matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setmatrix<-function(solve) m<<- solve
  
  getmatrix<-function() m
  
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


#cacheSolve returns inverse of matrix if value is not cached

#

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
      return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
