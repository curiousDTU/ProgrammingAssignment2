## Finding inverse of a very large matrix takes longer time to get solved
## so it is always useful to chache the results from the previous calculations.
## If no modifications are made to the matrix it saves lots of time doing calculations.

## makeCacheMatrix function caches the matrix and its inverse using the in-built 
##function solve().
cachematrix<-function(x = matrix()){
makeCacheMatrix <- function(x=matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- setinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve returns the cached inverse from the function makeCacheMatrix
## if the inverse of the matrix exists and is not null else it recomputes the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
result<-makeCacheMatrix(x)
cacheSolve(result)
}
