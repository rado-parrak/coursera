## Function describtion:
## makeCacheMatrix: creates an object from a matrix, eligible for cacheSolve
## cacheSolve: returns an inverse matrix to a matrix passed by an object created in "makeCacheMatrix"


## makeCacheMatrix.R:
## Inputs: x : a square matrix
## Ouptuts: x : an object with methods: 
## $setOriginal - saves an original input matrix to the object
## $getOriginal - returns original input matrix "x"
## $setInverse - saves an inverse matrix to matrix "x"
## $getInvers - returns an iverse matrix of matrix "x"

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


## casheSolve.R
## inputs: x: an object from function makeCacheMatrix
## outputs: inv - returns cached iverse matrix to the matrix "x", in case that "x" is known
##          invComp - returns computed inverse matrix to "x" and stores it to cache.

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
