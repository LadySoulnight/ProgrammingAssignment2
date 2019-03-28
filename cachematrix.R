## This function creates a matrix-like structure, used to cache its inverse. It consists of four methods,
## which can be used to set and get an R-matrix, as well as its inverse.
##
## The structure provides the following methods:
## - set(x): Stores the provided R-matrix (x) and sets the cached inverse to NULL.
## - get(): Returns the stored R-matrix.
## - setinverse(i): Stores the provided inverse (i). Make sure that it corresponse to the previously stored
##   matrix.
## - getinverse(): Returns the stored inverse.

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


## This function calculates the inverse of the provided matrix. It is to be called with the aforementioned
## matrix-like structure (created by makeCacheMatrix).
## If the provided matrix already contains its inverse, the cached inverse is returned directly (marked by 
## a comment).
## If no inverse is cached, the inverse is calculated and stored within the matrix's cache to speed up further
## invocations.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
