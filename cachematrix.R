## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

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
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
## note: this function will try to load corpcor library and if it's not installed will try to install the library

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
