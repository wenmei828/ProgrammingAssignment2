## write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## Solution
x<-makeCacheMatrix(matrix(5:8,2))
> x$get()
[,1] [,2]
[1,]    5    7
[2,]    6    8
> cacheSolve(x)
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> cacheSolve(x)
getting cached data
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> y<-cacheSolve(x)
getting cached data
> x$get()%*%y
[,1]         [,2]
[1,]    1 3.552714e-15
[2,]    0 1.000000e+00
