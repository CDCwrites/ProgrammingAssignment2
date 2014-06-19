##
# Cache potentially time-consuming computations of matrix inverse operations 
#
# Repeated computations (e.g. in a loop) of unchanged matrices are retrieved 
# from the cache instead of being re-calculated 
#
# Lexical scoping rules apply 
#       makeCacheMatrix     creates a cache of a matrix and its inverse 
#       cacheSolve          calculates the matrix inverse, if not cache'd
#
##

makeCacheMatrix <- function(x = matrix()) {
    ## Create cache of the previously-computed inverse of a matrix
    ## for future retrieval; original matrix is also stored.
    ##
    ## IN:  a matrix object, assumed invertible
    ## OUT: list containing matrix operations (function calls)
    ##
    ## This is modeled on makeVector function provided in Programming Assignment 2
    ## of the rprog-004 R Programming class from Coursera (JH)
    ##            
        
    m <- NULL           ## local to makeCacheMatrix
    
    ## define set, get, setinverse, getinverse fct's for matrix object
    ## function names are stored in returned list object
    set <- function(y) {
        x <<- y         ## save matrix data with non-local object env
        m <<- NULL      ## initialize cached 'm' var in non-local object env; NULL: uncached 
    }
    
    get <- function() x ## retrieve matrix data from object
    
    ## calculate the inverse of the matrix and store it in non-local object env
    setinverse <- function(solve) m <<- solve 
    
    ## retrieve the inverse of the matrix from the calling object env
    getinverse <- function() m
    
    ## create a list of function calls for setting and retrieving
    ## the matrix and the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


cacheSolve <- function(x, ...) {
    
    ## cacheSolve calculates and stores the inverse of the special "vector" created with the makeCacheMatrix. 
    ## It retrieves that inverse if it has already been calculated. 
    ##
    ## This is modeled on cachemean function provided in Programming Assignment 2
    ## of the rprog-004 R Programming class from Coursera (JH)
    ##    
    m <- x$getinverse() ## m is NULL if we haven't calc'd the inverse yet
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()     ## get the original matrix
    m <- solve(data)    ## compute the inverse of the matrix
    x$setinverse(m)     ## store the inverse of the original matrix in obj env
    m                   ## return the inverse of the matrix
}
