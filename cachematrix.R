

## Creating the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # sets initial inverse of x to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  #set the inverse matrix to null when input is reset
  }
  get <- function() x  ##retreive matrix
  
  setInverse <- function(solve) m <<- solve  ##inverse the matrix
  getInverse <- function() m   ##retreive inverse matrix
  
  list(set = set, get = get,   # list of functions to be returned
       setInverse  = setInverse ,
       getInverse = getInverse )
}






## Caching the inverse matrix.  If the inverse exist, take the inverse from cache. 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {     ## if an inverse matrix exists, return the inverse and notify users
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## retreive matrix
  m <- solve(data, ...)  ##inverse the matrix
  x$setInverse(m)   ## cache the inverse matrix
  m  #return inverse
}
