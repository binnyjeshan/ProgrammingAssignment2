## Put comments here that give an overall description 
## of what your functions do

## creates a matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## init inverse
  i<-NULL
  ## Method to set the matrix
  set<-function( matrix ) {
    m<<-matrix
    i<<-NULL
  }
  ## method to retrieve matrix
  get <- function() {
    m
  }
  ## do inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## get the inverse of the matrix
  getInverse <- function() {
    i
  }
  ## give back a list of the methods (override)
  list(set=se,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function makes the inverse of matrix returned
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Inverse of matrix 'x'
  m <- x$getInverse()
  ## Bail out if the inverse is not set
  if(!is.null(m)){
    message("error on data")
    return(m)
  }
  ## Get the matrix from x
  data<-x$get()
  ## inverse using matrix multiplication (solve func)
  m <- solve(data)%*%data
  ## inverse to m
  x$setInverse(m)
  ## Return m as the result
  m
}
