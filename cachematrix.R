## Here we are trying to cache the inverse of the square invertible matrix
## for that we are creating two functions one is the cache matrix and other
## function is to solve the inverse of the matrix

## This function caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 mr <- NULL
 set <- function(y) {
   x <<- y
   mr <<- NULL
 }
 get <- function() x
 setrev <- function(rev) mr <- rev
 getrev <- function() mr
 list(set = set,get = get, setrev = setrev , getrev = getrev)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mr <- x$getrev()
  if(!is.null(mr)){
    message("getting cached data")
    return(mr)
  }
  mat <- x$get()
  mr <- solve(mat, ...)
  x$setrev(mr)
  mr
}
