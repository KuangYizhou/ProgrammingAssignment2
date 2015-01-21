## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets the value of m to NULL
  y <- NULL # sets the value of y to NULL、
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y 
    m <<- NULL 
  }
  
  getmatrix=function() x
  setinverse<- function(inverse) m<<-inverse
  getinverse<- function() m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
cacheSolve <- function (x, ...) {
  # Need to compare matrix to what was there before!
  m <- x$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(m))# check to see if cacheSolve has been run before
    { 
    message("getting cached data")# check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
      return(m)
    }
    # otherwise 
    y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
    x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
    m <- solve(y, ...) # compute the value of the inverse of the input matrix
    x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    m # return the inverse
  }
