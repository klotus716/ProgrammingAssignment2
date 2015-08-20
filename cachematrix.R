## Two functions that store a matrix and cache its inverse for later use. 
## The special "matrix" object created by the makeCacheMatrix function is 
## a list used as the argument for the cacheSolve function. Note: these 
## functions assume that the matrix supplied is always invertible.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # No inverse of the matrix supplied has been calculated or cached yet.
  inv <- NULL

  # Define functions to set and get values of the matrix and its inverse.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  # Create object containing the above-defined set and get functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Computes the inverse from a special "matrix" object.

cacheSolve <- function(x, ...) {
  
  # If the inverse was already calculated, use the inverse in the cache.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # The inverse was not already calculated. It is now computed and cached.
  matrix1 <- x$get()
  inv <- solve(matrix1, ...)
  x$setinverse(inv)
  inv
}
