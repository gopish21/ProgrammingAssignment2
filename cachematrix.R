## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The following function is used to create a special "matrix". It contains a list of functions to
##1. set the value of matrix
##2. get the value of matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 
        z <- NULL
        set <- function(y)
        {
                x <<- y
                z <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse)  z <<- inverse
        getinverse <- function() z
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function

##The following function cacheSolve calculates the inverse of the special "matrix" created with the above function, 
##makeCacheMatrix. It first checks to see if the inverse of the matrix has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the
##inverse of the data and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
