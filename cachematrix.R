## R Programming Assignment 2


## Code for making the Cached Matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) minv <<- solveMatrix
  getInverse <- function() minv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Code for making the inversion of the Cached Matrix produced by the previous function

cacheSolve <- function(x, ...) {
    minv <- x$getInverse()
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setInverse(minv)
  minv      
}

