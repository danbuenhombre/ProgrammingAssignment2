## These two functions can be used to 'cache' the inverse of a matrix.
## If the inverse of the matrix given to the function has already been 
## calculated, it will return the already calculated value.
##
##
## 
## makeCacheMatrix will create a list containing a function to
## set the value of the matrix, get the value of the vector,
## set the value of the inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the mean of the special matrix created with the makeCacheMatrix function. 
## If the inverse has already been calculated, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the given matrix and sets the value of the inverse in the cache 
## using the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
