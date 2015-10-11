## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(nrow = nrow(x), ncol = ncol(x))
  set <- function(y){
    x <<- y
    m <<- matrix(nrow = nrow(y), ncol = ncol(y))
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  sum_m <- sum(m)
  if(!is.na(sum_m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <-  solve(data, ...)
  x$setinverse(m)
  m
}
