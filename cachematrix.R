## Contains functions for creating a special matrix which can store a matrix and its inverse 
## and also cache the inverse of the matrix 

## This function returns a list of functions for setting and getting the value of the matrix 
## and also declares functions for creating, setting and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #inverse of x
  set <- function(y) {
    if( !all.equal(x,y) ) {
      m <<- NULL
    }
    x <<- y    
  }
  get <- function() x
  getinverse <- function() m
  setinverse <- function(inv) m <<- inv  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of matrix present in special matrix "x" if it exists in cache.
##If not in cache, it computes the inverse of the matrix and returns

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
