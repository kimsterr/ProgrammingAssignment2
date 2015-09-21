## These functions are used to get the inverse of a matrix multiple times while
## limiting the computation required by caching the inverse.

## This function takes in a matrix x and returns a list of functions:
##      set:  changes the matrix referred to by x to the passed in new_matrix
##      get:  returns current matrix referred to by variable x
##      setinverse:  changes the cached matrix inverse to the parameter new_inv
##      getinverse:  returns the current cached matrix inverse or NULL if no inverse is
##                  currently cached
makeCacheMatrix <- function(x = matrix()) {
    # inv is the cached matrix inverse or NULL if uninitialized
    inv <- NULL
    
    set <- function(new_matrix) {
        x <<- new_matrix
        # Reset the inverse to NULL because we now have a new matrix
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(new_inv) inv <<- new_inv
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes and returns the inverse of a matrix represented
## by a special object returned by makeCacheMatrix, given that the inverse is not 
## already cached. Otherwise, the function simply returns the cached inverse 
## without needing to compute it again.
cacheSolve <- function(special_obj, ...) {
    inv <- special_obj$getinv()
    
    # Inverse already cached; no need to recompute it
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    ## Return a matrix that is the inverse (after caching it)
    the_matrix <- special_obj$get()
    inv <- solve(the_matrix)
    special_obj$setinv(inv)
    inv
}
