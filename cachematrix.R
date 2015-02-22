## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    # setOriginal    
    setOriginal <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    # getOriginal    
    getOriginal <- function() x
    
    # setInv
    setInverse <- function(inverse) inv <<- inverse
    
    # getInv
    getInverse <- function() inv    
    
    # create a list
    list(setOriginal = setOriginal, getOriginal = getOriginal,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    M <- x$getOriginal()
    invComp <- solve(M)
    x$setInv(invComp)
    return(invComp)
}
