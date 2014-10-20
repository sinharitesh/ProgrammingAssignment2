# There are a couple of functions whose objective is to cache 
# the inverse of a matrix. If a matrix is supplied before, these
# functions will supply them from memory and if it has been not,
# then a fresh inverse will be created and maintained in the memory for reuse.



# makeCacheMatrix provides the copy from the memory if exists
# otherwise it keeps the copy in the m variable.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# cacheSolve - this function gets the output from makeCacheMatrix's 
# getinverse function and compares if it is a null. 
# when it is null, it calculates and sets the result through setinverse function.
# if there is already a value, then it supplies the value to the caller
# without doing the computation.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
