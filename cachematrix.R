##
## This file contains two functions that can be used to create
## special object that can store a matrix and cache its inverse.
## Note: it is always assumed that the supplied matrix is invertible.
##

## This function creates a special "matrix" object with additional
## functionality. Effectively, it is list that wraps a matrix
## and provides the following functions:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
##
## example usage:
##
##  > m <- matrix(c(4,3,3,2), nrow=2,ncol=2)
##  > cachebleMat <- makeCacheMatrix(m)
##
makeCacheMatrix <- function(x = matrix()) {
  iMat <- NULL
  set <- function(y) {
    x <<- y
    iMat <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inv) iMat <<- inv
  getInv <- function() iMat
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes and caches the inverse of a matrix
## created with the previous function (makeCacheMatrix)
## If the inverse has already been computed it returns that
## inverse, otherwise it first computes the inverse and then
## returns the result.
##
## Attention: 
## You must pass a matrix created with makeCacheMatrix as 
## an argument to this function, NOT the original matrix.
##
## example usage:
##
##  > cacheSolve(cachebleMat) 
##
cacheSolve <- function(x, ...) {
  iMat <- x$getInv()
  if ( !is.null(iMat))
  {
    message("getting cached inverse matrix")
    return(iMat)
  }
  
  data <- x$get()
  iMat <- solve(data, ...) ## compute the inverse here
  
  x$setInv(iMat)
  
  ## Return the matrix that is the inverse of 'x'
  iMat
}
