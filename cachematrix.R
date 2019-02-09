#This two functions take advantage of R's lexical scoping
#to reuse an existing calculation, in this instance,
#of the inverse of a matrix, instead of calculating
#it from scratch

#This first function creates a list using a matrix object
#defined as an input. This list contain four functions
#to be used by the second function that actually calculates
#the inverse or retrieves it if calculated previously.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse= getinverse)
}


#As specified before, this function takes as an argument
#an objet of the type makeCacheMatrix, which is a list that
#contains four different functions. Then it makes use
#of those functions and calculates the inverse of a matrix
#previously specified as an input in makeCacheMatrix, or
#retrieves it if calculated previously.
cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}