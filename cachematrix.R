## This first function, makeCacheMatrix, creates a special "matrix" object that 
## can cache its inverse. This function creates a special "matrix", which is 
## really a list containing a function to: 
## 1. set the values of the matrix, 
## 2. get the values of the matrix,
## 3. set the value of the inverse, 
## 4. get the value of the inverse.
## I started with copying the example about caching the mean of a vector.
## Then I changed "m" into "inv", "mean" into "inverse", "setmean" into
## "setinv" and "getmean" into "getinv".

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The second function, "cacheSolve", calculates the inverse of the special 
## "matrix" created with the first function above. However, it first checks to 
## see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the values of the inverse in the cache via 
## the function "setinv".
## I started with copying the example about calculating the mean of the special 
## "vector". Then I changed "m" into "inv", "getmean" into "getinv", "mean" 
## into "solve" and "setmean" into "setinv".


cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
