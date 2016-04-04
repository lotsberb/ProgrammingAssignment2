# This file contains a function to create a matrix object capable of caching
# its own inverse and a second function to help facilitate the use of the
# cached-inverse matrix objects created by the first function

# This function accepts a matrix parameter and returns an object consisting
# of a list of functions.  Those functions set and get the base matrix, and
# set and get the inverse of the matrix with that inverse being cached 
makeCacheMatrix <- function(x = matrix()) {
    # Initially set the inverse to NULL when 
    inv <- NULL
    
    # This function is essentially equivalent to passing a matrix into
    # the makeCacheMatrix() function initially but can be called on an
    # existing object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # This function just returns the originally passed-in matrix
    get <- function() x
    
    # This function sets the inverse of the matrix, to be remembered (cached)
    # in the "inv" variable
    setinverse <- function(inverse) inv <<- inverse
    
    # This function returns the cached inverse
    # (which could be NULL if not yet set)
    getinverse <- function() inv
    
    # The list of accessible functions in this object, specified last so they
    # form the return value of makeCacheMatrix()
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function accepts an instance of a cached-inverse matrix object as
# as created by makeCacheMatrix() and returns the inverse of the matrix.
# If the object already has a cached inverse, that is returned, otherwise
# the inverse is calculated and stored in the passed-in object for future
# use.
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse from the given object x    
    inv <- x$getinverse
    
    # If the inverse obtained is *not* NULL, that means the given object
    # previously had its inverse calculated and stored, so we just have
    # to return it (extra message printed as per the Coursera assignment
    # example)
    if( !is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If we get here, the 'x' object passed in does not yet have its
    # inverse calculated and stored so do that now
    
    # Obtain the original matrix data
    data <- x$get()
    
    # Calculate the inverse and store in our local 'inv' variable
    inv <- solve(data)
    
    # Store the calculated inverse in the 'x' object passed to us
    x$setinverse(inv)
    
    # Finally, return the calculated inverse to the caller
    inv
}
