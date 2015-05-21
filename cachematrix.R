## a pair of functions that cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from the
## cache.
##

##The first function, makeVector creates a special "vector", which is really a list containing a function to
##    set the matrix
##    get the matrix
##    set the inverse
##    get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # using`<<-` assigns a value to an object in an environment
    # different from the current environment.
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the “matrix” returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache directly.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()

  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skip recalculating
    message("getting cached data")
    return(inv)
  }

  # otherwise, calculates the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)

  # sets the value of the inverse in the cache by the setinv function.
  x$setinv(inv)

  return(inv)
}
