################################################################################
##                                                                            ##
## pair of functions that compute & cache the inverse of a matrix             ##
##                                                                            ##
## makeCacheMatrix: create a matrix object with a cache for the inverse       ##
## cacheSolve: compute the inverse if it hasn't been computer & cached before ##
##             and return the inverse matrix                                  ##
##                                                                            ##
## author: Christopher Jung                                                   ##
## date: 19 June 2016                                                         ##
##                                                                            ##
################################################################################



## function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise the cached inverse matrix object to NULL  
    inv <- NULL
    
    ## create the function to set the matrix
    set <- function(y){
        ## set the value of the matrix 
        x <<- y
        
        ## new matrix value, so inverse has not been calculated yet
        ## set the cached inverse matrix to NULL
        inv <<- NULL
    }
    
    ## create function to return the value of the matrix
    get <- function() x
    
    ## create function to set the cached inverse matrix value
    setinv <- function(i) inv <<- i
    
    ## create the function to get the cached inverse matrix value
    getinv <- function() inv
    
    ## return the list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## returns the inverse of the special "matrix" returned by makeCacheMatrix
## if inverse has already been calculated, return it from cache

cacheSolve <- function(x, ...) {
    ## get the currently cached inverse matrix
    inv <- x$getinv()
    
    ## if i not NULL, the inverse has been cached
    if(!is.null(inv)) {
        message("returning cached data")
        return(inv)
    }
    
    ## i is NULL, the inverse does not exist and needs to be computed
    
    ## get the matrix
    matrix <- x$get()
    
    ## compute the inverse
    inv <- solve(matrix, ...)
    
    ## put the inverse matrix into cache
    message("returning newly computed data")
    x$setinv(inv)
    inv
}