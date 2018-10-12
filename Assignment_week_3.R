## this function will create a matrix inversion

makeCacheMatrix <- function(x = rebind (c(1, -1/4), c(-1/4, 1))) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    get(x)
  }
  get <- function(m = makeCacheMatrix(x)) x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this is the function that allow to cache the inverse matrix

cacheSolve <- function(x) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  data <- x$get(i)
  i <- solve(data)
  x$setinverse(i)
  inv
}