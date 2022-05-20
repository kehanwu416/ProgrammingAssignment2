## The "makeCacheMatrix"  creates a special "vector", which contains a function 
## to get/set the vector and the inverse

## The cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse is already in the cache, the function will directly pull the inverse from cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x<<- y
        m<<-NULL
    } ## set the value of the vector

get <- function()x  ## get the value of the vector
setinverse <- function(inverse) m<<- inverse  ## set the value of the inverse
getinverse <- function()m  ## get the value of the inverse

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


cacheSolve <- function(x, ...) {

     m <- x$getinverse()

       if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } ## if m exist in cache, return cached data

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
