## With the following functions the inversion of a matrix can be cached. 
## They do not consider invertibility of a matrix.

## function that can store additionally to a matrix its inversion
makeCacheMatrix <- function(x = matrix()) {
  
  # storage object for inverse matrix 
  m <- NULL
  
  # function to overwrite matrix and delete cached inversion
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # function to return matrix 
  get <- function() x
  
  # function to set inversion in parent environment
  setinverse <- function(inversion) m <<- inversion
  
  # function to retreive inversion
  getinverse <- function() m
  
  # return list with all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  function to retreive inversion of a matrix
cacheSolve <- function(x, ...) {
  
  # retreive inverse matrix
  m <- x$getinverse()
  
  # if inversion of matrix cached
  if (!is.null(m)) {
    
    # throw message and return inversion
    message("getting cached inversion of matrix")
    return(m)
    
  }
  
  # get matrix
  data <- x$get()
  
  # calculate inversion 
  m <- solve(data, ...)
  
  # save inversion of matrix in cache
  x$setinverse(m)
  
  # return inversion 
  m
}
