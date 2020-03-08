## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
## This enables us to avoid having to redo the potentially time-consuming computation of the matrix inverse,
## in case it has already been computed previously.

## Given a matrix x, creates a special "matrix", which is really a list containing a function to:
## set and get the value of a matrix and its inverse 
## (we assume all input matrices are invertible)
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse) matrix_inverse <<- matrix_inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" x created with the above function. 
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
