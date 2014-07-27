# makeCacheMatrix calculates the inverse of a matrix 'x' that is supplied to the 
# function and also stores the inverse value in the cache. This returns a list object
# containing various methods. cacheSolve function checks for a cached value if it 
# already exists if not calcualtes it and returns the inverse.

# Creating an empty matrix to store the value of the inverse.
# Saving the matrix to x and the inverse matrix to m.
# Returning a list containing the methods for setting the input matrix,returning
# the input matrix,setting the inverse matrix and returning the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        set_mat <- function(solve) {m <<- solve}
        get_mat <- function() {m}
        list(set = set, get = get,
             set_mat = set_mat,
             get_mat = get_mat)
}


# The function is to get the inverse matrix from the list object returned by makeCacheMatrix.
# Checks if the inverse value is existing cached already, and if yes then returns the cached value.
# If not, the function calculates the inverse for the matrix for the matrix 'x' saves the cached value
# and returns the inverse of 'x'.

cacheSolve <- function(x, ...) {
        m <- x$get_mat()
        if(!is.null(m)) {
                message("get cached data")
                return(m)
        }
        z <- x$get()
        m <- solve(z, ...)
        x$set_mat(m)
        m
}
