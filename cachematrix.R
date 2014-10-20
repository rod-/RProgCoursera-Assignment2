## makecachematrix creates an object from an input matrix which may contain and store the inverse of that matrix, so as to cache this information in case of repeated accesses to it in future.
## cachesolve will return the inverse of a matrix, solving for it mathematically IFF it has not already done so, or extracting the cached solution otherwise.

## Make a matrix object which can contain a stored cache of its inverse
makecachematrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Return a matrix that is the inverse of 'x'
cachesolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
