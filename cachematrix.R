makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## inverse version of x
    
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        y <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    ## get the inverse of the matrix
    getInverse <- function() inv
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  ## get the inverse of x
    
    ## check if the inverse has been already computed
    if(!is.null(inv)) {
        message("getting cached inverse version!!!")
        return(inv)
    }
    
    ## compute the inverse version
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv
}
