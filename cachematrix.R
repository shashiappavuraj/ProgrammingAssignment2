
#Computing inverse of the matrix and storing in cache
#solve function is used to compute the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Getting the inverse of the matrix from Cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverseun(m)
  m
}
