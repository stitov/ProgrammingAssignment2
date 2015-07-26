# This functions help to compute inverse matrix and make it faster 
# for unchanged data by using cache

# The number of functions to create and store the original and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Set the new matrix
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    # Retrieve the original matrix
    get <- function ()
        x
    
    # Assign to variable m inverse matrix
    set_solved <- function (solved)
        m <<- solved
    
    # Retrieve the computed inverse matrix
    get_solved <- function()
        m
    
    list(
        set = set, get = get,
        set_solved = set_solved,
        get_solved = get_solved
    )
}

# Compute the inverse matrix and store result
cacheSolve <- function (x, ...) {
    # Retrieve the computed inverse matrix
    m <- x$get_solved ()
    
    # If not null return computed matrix and end function
    if (!is.null (m)) {
        message ("getting cached data")
        return (m)
    }
    
    # Retrieve the original matrix
    data <- x$get()
    
    # Solve matrix
    m <- solve (data, ...)
    
    # Assign the result 
    x$set_solved (m)
    m
}
