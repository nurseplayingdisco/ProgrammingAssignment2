## The two functions allows caching of a matrix and its inverse. After defining an object with
## makeCacheMatrix, $get() and $set() can be used to set and access a matrix in the object. 
## Through the cacheSolve function, the matrix in the object is inversed unless the inversion 
## was already performed, when it accesses the stored inversed data.
## The inverse can also be accessed with $getinverse(). 

## makeCacheMatrix: This function creates an object containing a matrix, and can cache 
## an inversion of the matrix contained.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function creates an inverse of the matrix defined in makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then it returns 
## the previously calculated inversed matrix. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'      
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  invmatrix <- x$get()
  m <- solve(invmatrix, ...)
  x$setinverse(m)
  m
}


