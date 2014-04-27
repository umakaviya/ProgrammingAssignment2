##The program calculates teh inverse of a matrix
##The value is stored in cache
##This attempts to give the cache value rather than recalculating again


## the below function gets the matrix as input
## It returns a list of 4 elements each of which is a function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##the below function calculates the inverse of the matrix
##If the value is already available in cache it gives this value
##rather than recalculating

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}