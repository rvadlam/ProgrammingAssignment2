## The makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(my_matrix = matrix()) {
        my_matrixinverse <- NULL
        set <- function(temp_matrix) {
                my_matrix <<- temp_matrix
                my_matrixinverse <<- NULL
        }
        get <- function() my_matrix
        setmatrixinverse <- function(matrixinverse) my_matrixinverse <<- matrixinverse
        getmatrixinverse <- function() my_matrixinverse
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
        
}


## The following function calculates the matrix inverse of the special 
## "matrix" created with the above function. However, it first checks to see if the 
## inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data using solve function.
## ginv function can also be used but MASS package needs to be loaded first
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_matrixinverse <- x$getmatrixinverse()
        if (!is.null(my_matrixinverse)) {
                message("getting cached data")
                return(my_matrixinverse)
        }
        data <- x$get()
        my_matrixinverse <- solve(data,...)
        x$setmatrixinverse(my_matrixinverse)
        return(my_matrixinverse)
}
