## Put comments here that give an overall description of what your
## functions do
## Function makeCacheMatrix has 4 list functions, set, get, setinversion, and getinversion
## Example: 
## >a = matrix(c(1,2,3,1),2,2)
## > f <- makeCacheMatrix(a)
## > > f$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    1
## >f$getinversion()
## NULL


## Write a short comment describing this function
## Function cacheSolve takes function makeCacheMatrix as argument and calculate the inversion of the matrix created by makeCacheMatrix.
## If the inversion is already calculated, cacheSolve will return the cached data
## If not, cacheSolve will calculate the inversion, cache the result, and return the result
## Example
## > cacheSolve(f)
##     [,1] [,2]
##[1,] -0.2  0.6
##[2,]  0.4 -0.2
##> f$getinversion()
##     [,1] [,2]
##[1,] -0.2  0.6
##[2,]  0.4 -0.2
##> cacheSolve(f)
##getting cached data
##     [,1] [,2]
##[1,] -0.2  0.6
##[2,]  0.4 -0.2

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinversion = function(inversion){ 
    if (identical(dim(inversion), dim(x))) {
    m <<- inversion }
    else(
      message("wrong input!")
    )
    }
  getinversion = function() m
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}

cacheSolve <- function(x, ...){
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
