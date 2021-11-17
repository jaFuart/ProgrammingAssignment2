## makeCacheMatrix is a function that creates a special 
## “matrix” object that can cache its inverse. At the beginning, initialize the 
## inverse property. Then creating two methods "set", that set the matrix, and
## "get", that get the matrix. After that creating a "sInv" and "gInv" methods,
## which set and output the inverse matrix respectively. In the end, it's 
## return a list of the methods

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  get <- function()x
  sInv <- function(inverse) {
    i <<- inverse
  }
  gInv <- function() {
    i
  }
  list(set = set, get = get,
       sInv = sInv,
       gInv = gInv)
}


## chaceSolve is a function that computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above. It's return a matrix that is the inverse 
## of 'x' or return the inverse if its already set. Then get the matrix from 
## the object and calculate the inverse using matrix multiplication. After that
## set the inverse to the object and return the matrix

cacheSolve <- function(x, ...) {
  m <- x$gInv()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$sInv(m)
  m
}
