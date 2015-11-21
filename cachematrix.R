makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## output is a list containing functions to
  ##            1. set the matrix
  ##            2. get the matrix
  ##            3. set the inverse
  ##            4. get the inverse
  ##         this list will be used as input to cacheSolve()
  
  inv<-NULL
  set<-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()
  ## output is inverse of original matrix and input to makeCacheMatrix()
  
  inv <- x$getinv()
  if (!is.null(inv)){
    # if already calculated, get it from cache & skip computation 
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  # if not calculated, calculates inverse 
  # sets value of inverse in cache via the setinv function
  x$setinv(inv)
  return(inv)
}

