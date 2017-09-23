## Put comments here that give an overall description of what your
## functions do
## This functin is written to illustrate the caching behaviour of R objects.
## This is a part of Coursea's assignment on R Programming written on 23rd Sep, 2017 by Biswajeet Sahoo


## Write a short comment describing this function
## This function will build a matrix object which can cache it's own reverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
      set <- function(y) {               ## define the set function to assign new 
          x <<- y                        ## value of matrix in parent environment
          inv <<- NULL                   ## if there is a new matrix, reset inv to NULL
      }
      get <- function() x                     ## define the get fucntion - returns value of the matrix argument
      
      setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
      getinverse <- function() inv                     ## gets the value of inv where called
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                    ## to the functions with the $ operator

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
      if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
