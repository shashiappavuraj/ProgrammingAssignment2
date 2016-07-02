
#Computing inverse of the matrix and storing in cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
#function to get the matrix
  getmatrix <- function() x
#function to set inverse of the matrix
#solve function is used to compute the inverse of the matrix
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m 
# Back a list of the methods 
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Getting the inverse of the matrix from Cache
cacheSolve <- function(x=matrix (), ...) {
  m <- x$getinverse()
#checking whether matrix inverse is present in cache
  if(!is.null(m)) {
    message("Getting cached Matrix Inverse")
    return(m)
  }
  data <- x$getmatrix()
#solve function is used to compute the inverse of the matrix
  m <- solve(data, ...)
#send the matrix to makeCacheMatrix() function to compute the matrix inverse and cache it
  x$setinverse(data)
  message("Matrix Inverse not present in Cache")
  m
}

#Testing Without Cache data
a<-makeCacheMatrix()
b= matrix(c(1,2,3,4), nrow=2, ncol=2)
a$setmatrix(b)
cacheSolve(a)

#Testing With Cache data by running the function again
cacheSolve(a)

