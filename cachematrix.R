## Since matrix inversion is a costly computation 
## We are writing two functions to cache previously computed inverse of a matrix,
## so we don't have to compute it every time. If it was previously computed,
## we just use the cashed value


## This function creates a special "matrix" object that can cache its inverse
## Input: object of type matrix. Default value: empty matrix
## Output: list of the following functions
## set: sets the value of the vector
## get: gets the value of the vector
## setinverse: sets the value of the inverse
## getinverse: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special matrix
## created with the "makeCacheMatrix" function. 
## it checks to see if the inverse is previously calculated it gets it from the cache
## Input: special "matrix" created with "makeCacheMatrix" function
## Output: inverse of a matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
