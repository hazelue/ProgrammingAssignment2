## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function is used to ctreate an object that stores matrix and calculate inverse before caching


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- inv
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##If the inverse has already been calculated,it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x = matrix(), ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
