## Put comments here that give an overall description of what your
## functions do

## create matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solveMatrix) inv <<- solveMatrix
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## calculate inverse of the matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv      
}