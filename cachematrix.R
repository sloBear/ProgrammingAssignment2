## Modification of Caching the Mean of a Vector example
## makeCacheMatrix prepares the components necessary components for creating and caching 
## the inverse of a square matrix.
## cacheSolve uses the components prepped in makeCacheMatrix to create the inverse matrix 
## and return that matrix.  cacheSolve returns the previous inverse if no changes in 
## the original matrix have occurred

## function takes matrix as argument and creates a list containing functions to:  set the matrix, retrieve
## that matrix, create the inverse of matrix x, and retrieve the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
        ##create null matrix
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function takes the matrix x and creates the inverse using solve().  The matrix must be square.
## The inverse of x is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting inverse matrix")
                return(m)
        }
        matdata <- x$get()
        m <-solve(matdata,...)
        x$setinverse(m)
        m
}
