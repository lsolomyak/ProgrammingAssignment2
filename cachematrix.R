
# these functions cache the inverse of a matrix to make it more easily accesible without requiring 
# repeat computation every time the function is called
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL  
      set <- function(y) {
            x <<- y  # the value that is input in the function (notice we are setting it to the global environment ) 
            m <<- NULL #  m has not yet been calculated (set to the global environment )
      }
      get <- function() x  # return x
      setinverse <- function(solve) m <<- solve  # solve is the function that will give us an inverse
      getinverse <- function() m       
      list(set = set, get = get,     #we set the list to make each element of the object accessible 
           setinverse = setinverse,
           getinverse = getinverse)
}


## This is where we actually solve the inverse of the matrix to be stored 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()               
      if(!is.null(m)) {                 
            message("getting cached data")   #if it was already solved before we don't need to solve it again
            return(m)             
      }
      data <- x$get()                  
      m <- solve(data, ...)      # if it wasn't solved before we solve it          
      x$setinverse(m)            # and then cache it so it can be accessible next time             
      m  
}
