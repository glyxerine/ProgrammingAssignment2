## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Variable to hold inverse matrix
        i <- NULL
        
        ## Set a new matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the matrix set
        get <- function() x
        
        ## Set the inverse matrix
        setinverse <- function(imatrix) i <<- imatrix
        
        ## Get the inverse matrix
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## Check if inverse matrix has already been solved
        if (!is.null(i)) {
                message("Retriving cached inverse matrix")
                return(i)
        }
        
        ## Get the matrix
        matrixdata <- x$get()
        
        ## Solve for the inverse matrix
        i <- solve(matrixdata)
        
        ## Cache the inverse matrix
        x$setinverse(i)
        
        ## Return the inverse matrix
        i
}