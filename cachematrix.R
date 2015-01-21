## Write a short comment describing this function
 
 makeCacheMatrix <- function(x = matrix()) {
-
+  m <- NULL # sets the value of m to NULL
+  y <- NULL # sets the value of y to NULL、
+  setmatrix <- function(y) { #set the value of the matrix
+    x <<- y 
+    m <<- NULL 
+  }
+  
+  getmatrix=function() x
+  setinverse<- function(inverse) m<<-inverse
+  getinverse<- function() m
+  
+  list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
+       setinverse = setinverse,
+       getinverse = getinverse)
+  
 }
 
 
 ## Write a short comment describing this function
 
-cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
-}
+cacheSolve <- function (x, ...) {
+  # Need to compare matrix to what was there before!
+  m <- x$getinverse() # if an inverse has already been calculated this gets it
+  if(!is.null(m))# check to see if cacheSolve has been run before
+    { 
+    message("getting cached data")# check that matrix hasn't changed,if hasn't, sends a text message and returns the cached matrix
+      #parts removed
+      return(m)
+    }
+    # otherwise 
+    y <- x$getmatrix() # get the value of the input matrix
+    x$setmatrix(y) # run the setmatrix function to cache it
+    m <- solve(y, ...)
+    x$setinverse(m) # cache the inverse
+    m # return the inverse
+  }
