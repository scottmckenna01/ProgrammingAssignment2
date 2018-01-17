## makeCacheMatrix and cacheSolve store a special matrix and calc the inverse
## 
## makeCacheMartix creates special Matrix object that can cache its inverse
##

## section creates matrix to set and get intial values and cache for
## calc'd inverse values
##

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
      x <<- y
      invmat <<- NULL
    }
    get <- function() x
##    
##  set/get for matrix inverse
##
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 
## Computes the inverse of the special matrix returned by makeCacheMatrix (above)
## If inverse alreacy calc and hasn't changed then cacheSolve retrieves inverse
## matrix from cache
##

cacheSolve <- function(x, ...) {
    invmat <- x$getinverse()
##
##  Check for cached values not NULL, returns cached values or calcs new inverse
##    
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    data <- x$get()
##
##  calc matrix inverse and cache
##
    invmat <- solve(data, ...)
    x$setinverse(invmat)
    invmat
}
