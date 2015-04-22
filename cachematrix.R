## cacheSolve and makeCacheMatrix will use caching in calculating the inverse of a matrix 

## makeCacheMatrix will create a list of functions used by cacheSolve
## input is cheked to be a square matrix

makeCacheMatrix <- function(x){
   
   if(!is.matrix(x)) stop("argument must be a matrix")
   if(!dim(x)[1] == dim(x)[2]) stop("matrix must be square")
   
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve will take a makeCacheMatrix as input and outputs the inverse
## if inverse is cached, cache is outputted
## if inverse not no cache, inverse is calculated, cached and outputted

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
