## Put comments here that give an overall description of what your
## functions do
## Takes a matrix that is passed in and provides it inverse and chaches
## this result for future use.

## Write a short comment describing this function
## allows a matrix to be set for the function and to retrieve it when needed(get)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## checks if changes have been made and eiter recalculates or pulls chached calc

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
