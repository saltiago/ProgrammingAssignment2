## Two functions to cache and return the inverse of an invertible matrix

##Creates a cached matrix (and list) for use with the second function
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function()x
   setInv <- function(solve)m <<- solve
   getInv <- function()m
   list(set=set, get=get, 
        setInv=setInv,
        getInv=getInv)
   
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   m <- x$getInv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInv(m)
   m
}

#Can test using:
#x <- matrix(rnorm(16),4,4); y <- makeCacheMatrix(x); cacheSolve(y)
#solve(x) #(to compare)
