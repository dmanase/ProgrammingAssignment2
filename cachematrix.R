## =============================================================================
## Two functions have been created in order to be able to create and reuse the 
## cached value of the inverse of a matrix x:
## - The makeCacheMatrix - creates a series of the set/get methods to handle x
## - cacheSolve          - calculates and returns the inverse of matrix x 
##                         if not available in cache and just returns it if 
##                         already cahed.
## =============================================================================

## -----------------------------------------------------------------------------
##
## makeCacheMatrix (x,...)
##
## Args:
##  x - square numeric matrix
##
## -----------------------------------------------------------------------------
## Function to create the set, get, reset and store/cache the inverse of a 
## matrix x (into mSolve).
## =============================================================================

makeCacheMatrix <- function(x = matrix()) {
    # New call, reset the old x inverse cached
    mSolve <- NULL
    # set X to the new matrix (y)
    set    <- function(y) {
        x <<- y
        # clear the old x inverse chached
        mSolve <<- NULL
    }
    # return the current matrix (x)
    get      <- function()    x
    # Store the Inv parameter into the new x inverse cache
    setSolve <- function(Inv) mSolve <<- Inv
    # Return the cached x inverse
    getSolve <- function()    mSolve
    
    # Create the return list object
    list(   set = set, 
            get = get,
            setSolve = setSolve,
            getSolve = getSolve
    )
}
#===============================================================================




## -----------------------------------------------------------------------------
##
## cacheSolve (x,...)
##
## Args:
##  x - square numeric matrix
##
## -----------------------------------------------------------------------------
## Function that calculates the inverse of a matrix and caches it (into mSolve) 
## using the methods associated with the object x by the makeCacheMatrix 
## function.
## =============================================================================

cacheSolve <- function(x, ...) {
    ## Get the current cached inverse of matrix x
    mSolve <- x$getSolve()
    
    # Check if it has been calculated (not null) and return it if yes.
    if (!is.null(mSolve)) {
        message("getting cached data")
        return(mSolve)
    }
    # If here the Inverse of x has not been calculated
    # Store the matrix x in the local variable data 
    data   <- x$get()
    # Calculate the inverse of data
    mSolve <- solve(data, ...)
    # Cache the inverse of x for future use
    x$setSolve(mSolve)
    # and return the inverse of x
    mSolve
}


#===============================================================================