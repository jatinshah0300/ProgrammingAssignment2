## The makeCacheMatrix function creates a closure containing a matrix and its inverse.
## It provides four functions to interact with the matrix and its inverse:
##   1. set_matrix: Set the matrix and reset the cached inverse.
##   2. get_matrix: Get the matrix.
##   3. set_inverse: Set the cached inverse of the matrix.
##   4. get_inverse: Get the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Variables to store the matrix and its inverse
    cached_inverse <- NULL
    
    # Function to set the matrix and reset the cached inverse
    set_matrix <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    
    # Function to get the matrix
    get_matrix <- function() {
        x
    }
    
    # Function to set the cached inverse of the matrix
    set_inverse <- function(calculated_inverse) {
        cached_inverse <<- calculated_inverse
    }
    
    # Function to get the cached inverse of the matrix
    get_inverse <- function() {
        cached_inverse
    }
    
    # Return a list of functions that can be used to interact with the cached matrix and its inverse
    list(set = set_matrix, get = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The cacheSolve function calculates the inverse of a matrix with caching for improved performance.
## It takes a closure object 'x' created by the makeCacheMatrix function as input.
## It first checks if the cached inverse of the matrix is available. If so, it returns the cached value.
## If the cached inverse is not available, it calculates the inverse of the matrix using the 'solve' function.
## The calculated inverse is then set as the cached inverse for future use.
## Finally, the function returns the calculated inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached_inverse <- x$get_inverse()
    
    # If the cached inverse is available, return it directly
    if (!is.null(cached_inverse)) {
        message("Getting cached data")
        return(cached_inverse)
    }
    
    # If the cached inverse is not available, calculate the inverse of the matrix
    data <- x$get_matrix()
    calculated_inverse <- solve(data, ...)
    
    # Set the calculated inverse as the cached inverse for future use
    x$set_inverse(calculated_inverse)
    
    # Return the calculated inverse
    calculated_inverse
}



