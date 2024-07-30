## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix provides a framework for getting and setting the matrix and its inverse from cache
makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## cacheSolve computes the inverse of a matrix using a cached version if available.
# If the inverse is already cached, it returns the cached value.
# If not, it calculates the inverse, caches it, and then returns the newly computed inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}