## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly. 
## The assignment 2 consists of two functions makeCacheMatrix and cacheSolve. 

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. Set the value to a matrix using
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix
## inv contains the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y      
                inv <<- NULL
        }
        
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function () inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## cacheSolve function finds the inverse of a matrix. It first checks whether the 
## inverse has been already calculated and is in the cache. If so, the function 
## returns the inverse from the cache and skips the computation. Otherwise it
## finds the inverse and saves the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## In R the function solve(X) returns the inverse of a square invertible matrix X
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Sample runs
## Example 1. Diagonal matrix
## > source("cachematrix.R")
## > x <- diag(3)
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## > cacheeSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

## Example 2. An invertible 2x2 matrix
## > A <- rbind(c(4,3), c(3,2))
## > A
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > m <- makeCacheMatrix(A)
## > m$get()
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


