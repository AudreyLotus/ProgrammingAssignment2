## My functions will create a special 'matrix' object that can
## cache its inverse. Then a function check whether the inverse
## has already ben calculated: if yes, retrive the inverse;
## if not, calculate and return the inverse.

## 'makeCacheMatrix' is a function that takes a 'matrix' argument
## and return a list of functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)

}


## 'cacheSolve' is a function that takes a 'list' argument returned
## by 'makeCacheMatrix' above and return the inverse of a matrix.
## If the inverse has been calculated it directly returns the result.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
