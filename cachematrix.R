## The first function makeCacheMatrix caches a matrix 
## The second - cacheSolve uses the first functions output to invert the matrix
## that was cached

## This is makeCacheMatrix 
## The function will...
## set the matrix - setI
## get the matrix - getI
## set the inverse of that matrix - setInv
## then get the inverse of that matrix - getInv and return a list of each

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invert <<- inv
  getInv <- function() invert
  list( set = set , get = get , setInv = setInv, getInv = getInv)
}

## This is the cacheSolve function
## this function will....
##get the inverse of the matrix cached in makeCacheMatrx
## it will compute the inverse of the matrix if it has not been computed yet
##if it has been computed, it will get the inverse from the cache

##There is an assumption of an invertible matrix

cacheSolve <- function(x, ...) {
  invert <- x$getInv()
  if(!is.null(invert)) {
    message("getting the cached inverse...")
    return(invert)
  }
  data<- x$get()
  invert <- solve(data)
  x$setInv(invert)
  invert
}	




