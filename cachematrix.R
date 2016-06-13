## Put comments here that give an overall description of what your
## functions do

# This pair of functions allows the caching of the inverse of a matrix.
# We assume all supplied matrices are invertible.

## Write a short comment describing this function

# This first function creates a special matrix object that can cache 
# the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

# This second function computes the special inverse of the matrix 
# returned by the makeCacheMatrix function above.
# If the inverse has already been computed it simply retrieves it
# rather than computing it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
