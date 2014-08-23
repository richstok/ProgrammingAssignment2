## Richard Stokotelny 2014-08-20
##
## initial prototype
## 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # clear m
  m <- NULL
  
  # return y as a matrix
  set <- function(y) {
    a <- matrix(y)
    l <- length(a)
    dim(a) <- c(l,l)
    x <<- a
    m <<- NULL
  }
  
  # return x
  get <- function() {
    x
  }
  
  # cache inverse matrix
  setinv <- function(inv) { 
    m <<- inv 
  }
  
  # return inverse matrix
  getinv <- function() {
    m
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverted matrix")
    return(m)
  }
  # get matrix
  data <- x$get()
  # invert matrix
  m <- solve(data, ...)
  # save inverse matrix
  x$setinv(m)
  m
  
}

