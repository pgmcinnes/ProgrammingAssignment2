## These functions, makeCacheMatrix and cacheInverse take an invertible matrix, take it's inverse 
## and then cache the resulting inverse.
##

## makeCacheMatrix 
## This function creates a list of "get" and "set" functions that will be available in the global environment
## makeCacheMatrix can and will compute the inverse of the matrix,x, and then pass that through the "setinverse" function
## This inverse, stored as "m_inv", and the additional functions can be used in cacheInverse or m_inv applied elsewhere
makeCacheMatrix <- function(x = matrix()) {
  
  
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function() m_inv <<- apply(x,c(1,2),solve)
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## cacheInverse uses the parent function getinverse definied in makeCacheMatrix to create m_inv. 
## If already created this will return the calculated m_inv. A value "data" will be used to calculate this same inverse, set it to m_inv and print the value
##

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- apply(data,c(1,2),solve)
  x$setinverse
  m_inv
  
  
  
}


