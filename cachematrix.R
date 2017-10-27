## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a Matrix object to cache its invrese


makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
 ## set value of matrix 
  set  <- function(y)
  {
    x <<- y
    cache <<- NULL
    
  }
  get <- function() x
##set the inverse of matrix
  setMatrix <- function(inverse) cache <<- inverse
  ## get the inverse
  getInverse <- function() cache
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

## Write a short comment describing this function
## actual inverse caluculation function for stored values in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- x$getInverse()
  if(!is.null(cache)) 
  {
    message("getting cached data")
    return(cache)
  }
  
  matrix <- x$get()
  
 
      cache <- solve(matrix, ...)
      x$setMatrix(cache)
      return(cache)
      
    }
 
