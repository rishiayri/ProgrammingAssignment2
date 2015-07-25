## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Function takes an invertible square matrix as input 
## return : a list containing functions to 
##  1. set the matrix
##  2. get the matrix
##  3. set the matrix
##  4. get the inverse
## this list is then used as input to the cacheSolve() function
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Cache solve to get the inverse
cachesolve <- function(x, ...) {
## Input : Output of makecachematrix()
## return : inverse of the original matrix input to makecachevector()
  inv = x$getinv()
  # if the inverse has already been calculated
  if(!is.null(inv)) {
    # fetch from cache and avoid computing it
    message("getting cached data")
    return(inv)
  }
  # else compute the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  ## set the value of the inverse in the cache via setinv function.
  x$setinv(inv)
  return(inv)
}
  



