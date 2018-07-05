## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# the function accepts a matrix argument
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  get <- function() x
 
  # set the inverse  
  setinv <- function(inverse) inv <<- inverse
  
  # get the inverse
  getinv <- function() inv
  # we used list here 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# function that output the makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

  # the next code validates if inverse has already been
  # calculated
  if(!is.null(inv)) {
  	#if calculated, it skips the computation and return the value from cache
    return(inv)
  }

  #otherwise, get the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  #return the inverse value
  inv
}
