
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ## Method to get the inverse of the matrix
  getinverse <- function() inv
  ## Return a list of the methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## Just return the inverse if its already set
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## Get the matrix from our object
  data <- x$get()
  inv <- solve(data)
  ## Set the inverse to the object
  x$setinverse(inv)
  inv
}
#run this function f
cacheSolve(m)