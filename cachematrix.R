## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Shown below are two functions that are used to create a object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix is the function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Function to get the matrix
  get <- function() x
  
  #Function to set the Inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #Function to get the Inverse matrix
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The function "cacheSolve" computes the inverse of the "matrix" created by above given
## makeCacheMatrix function. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  #Case1:Inverse exists in cache
  if (!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  #Case2: Inverse is NULL
  else{
    message("Matrix changed. Calculating Inverse")
    mat <- x$get() # get changed matrix
    inv <- solve(mat,...) # solves and obtains the inverse using solve.
    x$setInverse(inv)# sets the value of the inverse in the cache.
  }
  
  #return inverse of matrix
  inv
  
}
