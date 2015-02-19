## The two functions below demonstrate the use to caching a calculated result
## This can be very helpful and will save computational effort while calculating
## the inverse of a matrix recursively


## makeCacheMatrix(argument a matrix) - creates a list of functions to set/get 
##                  values for the matrix and inverse of that matrix which has been
##                  passed as an argumernt
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y = matrix()){
          x <<- y
          i <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) i  <<- inverse
   getinv <- function() i
   list(set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
        )
}



## cacheSolve(argument should be an object of 'makeCacheMatrix') 
##    - calculates the inverse of the matrix if it is not already present calculated
##    and stored in 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)){
        message("Getting cached inverse matrix")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinv(i)
    i
}
