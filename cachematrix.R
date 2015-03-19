## This script defines a function that will initialize a special matrix with
## properties to cache its inverse, as well as a function to retrieve the
## cached inverse or compute and cache one if it does not already exist.

## makeCacheMatrix: This function creates a special "matrix" object (called the cache matrix) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL    ## Reset the cached inverse of the input matrix to NULL
  set <- function(y) {    ## Set the cache matrix to be equal to the input matrix
    x <<- y    ## Assign the value of the input matrix 'y' to the cache matrix 'x' in the parent environment
    x_inv <<- NULL    ## Reset the cached inverse (in the parent environment) of the cache matrix to NULL
  }
  get <- function() x    ## Return the value of the cache matrix
  setinv <- function(inverse) x_inv <<- inverse    ## Cache the inverse for the cache matrix
  getinv <- function() x_inv    ## Retrieve the cached inverse for the cache matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)    ## Return a list of methods that can be used with the cache matrix object
}


## cacheSolve: This function computes the inverse of the special "matrix" (cache matrix, 'x') returned by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  
        
  x_inv <- x$getinv()    ## Retrieve the cached inverse from the cache matrix, 'x'
  if(!is.null(x_inv)) {    ## If the cached inverse exists (is not NULL), then display a message, return the cached inverse and exit the function
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()    ## If the cached inverse does not exist (is NULL), then assign the value of the cache matrix, 'x', to the temp variable 'data'
  x_inv <- solve(data, ...)    ## Determine the inverse of the matrix 'data'
  x$setinv(x_inv)    ## Set the cached inverse of the cache matrix to the inverse computed in the line above
  x_inv  ## Return a matrix that is the inverse of 'x'
}