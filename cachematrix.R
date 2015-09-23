
## Pair of functions that return a matrix inverse of x.


## makeCacheMartrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## begins by setting the inverse to NULL as a placeholder for a future value.
  inver_m <- NULL 
  
  ## defines a function to set the vector, x, to a new vector, y, and resets the inverse, inver_m, to NULL
  set <- function(y) {
    x <<- y
    inver_m <<- NULL
  } 
  
  ## returns the vector, x
  get <- function() y 
  setinverse <-
    function (inverse)
      inver_m <<- inverse ## sets the inverse, inver_m, to inverse
  
  ##  returns the inverse, inver_m
  getinverse <- function() inver_m 
  
  ## returns the 'special vector' containing all of the functions just defined
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse) 
}


##  This function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. .
## If the inverse has already been calculated then the cachesolve
##  should take the inverse from the cache.

cacheSolve <- function(x, ...) {
  inver_m <- x$getinverse()
  if (!is.null(inver_m)) {
    message("getting cached inverse matrix")
    return(inver_m)
  }
  else {
    m <- x$get()
    inver_m <- solve(m)
    x$setinverse(inver_m)
    return(inver_m)
    ## Return a matrix that is the inverse of 'x'
  }
}