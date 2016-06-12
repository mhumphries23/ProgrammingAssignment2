## Code to cache the inverse of a matrix
## and return it if called for

## Set and get value of matrix and set and get inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # x: a square, invertible matrix
  # return: a list of functions to set and get the matrix
  # & set and get the inverse. This is cacheSolve() input
   
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <-function(inverse)
  inv <<- inverse
  getinv <- function() inv
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


## Calculate inverse of vector created above if it has not already been calculated, otherwise return value from cache

cacheSolve <- function(x=matrix(), ...) {
  inv<-x$getinv()
  if(!is.null(inv))
    {
    # Get from cache
      message("getting cached data")
      return(inv)
    }
  my.matrix<-x$get()
  inv<-solve(my.matrix, ...)
  x$setinv(inv)
  return(inv)
}
