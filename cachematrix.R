## The purpose of the first function is to create a "special" matrix that
## will cache it's own inverse. The second function will examine the matrix
## and determine if its inverse was cached and return it, If not it will
## solve and cache the inverse. 

## makeCacheMatrix takes as its input a matrix and creates a cache
## to store the inverse of the matrix in. 
## Usage: a <- makeCacheMatrix(x = matrix()) creates a CacheMatrix 'a'
## a$get()  will return the matrix
## a$set() will change the matrix
## a$getinverse() will return the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<-y
                i <<-NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve takes the special matrix created from makeCacheMatrix as
## its input. It then searches the cache of the matrix and determines if
## the inverse was already calculated. If so, it gives the inverse. If
## the inverse was not pre-calculated, it calculates the inverse and
## stores it in the cache of the input matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
}
