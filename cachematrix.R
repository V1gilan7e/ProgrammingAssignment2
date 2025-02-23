## These functions work together to cache the inverse of a matrix.
## 'makeCacheMatrix' creates a special "matrix" object that can store its inverse.
## 'cacheSolve' computes the inverse of the matrix; if it has already been calculated,
## then it retrieves the inverse from the cache to save computation time.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse cache to NULL

    set <- function(y) {
        x <<- y      # Set the matrix value in the parent environment
        inv <<- NULL # Reset the inverse cache since the matrix has changed
    }

    get <- function() {
        x  # Return the current matrix
    }

    setInverse <- function(inverse) {
        inv <<- inverse  # Cache the inverse of the matrix
    }

    getInverse <- function() {
        inv  # Retrieve the cached inverse (if available)
    }

    # Return a list of functions to interact with the matrix and its inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse is already cached, it returns that value with a message.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()  # Check if the inverse is already cached

    if (!is.null(inv)) {
        message("getting cached data")  # Inform the user that cached data is being used
        return(inv)  # Return the cached inverse
    }

    mat <- x$get()        # Retrieve the matrix
    inv <- solve(mat, ...)  # Compute the inverse using the solve() function
    x$setInverse(inv)     # Cache the computed inverse for future use
    inv                   # Return the inverse matrix
}

#ex-
# Create a matrix
m <- matrix(c(2, 2, 1, 4), 2, 2)

# Create the special matrix object
m_cache <- makeCacheMatrix(m)

# Compute and cache the inverse
inv1 <- cacheSolve(m_cache)
print(inv1)

# Retrieve the cached inverse
inv2 <- cacheSolve(m_cache)
print(inv2)
