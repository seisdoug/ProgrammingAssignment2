## The functions  aCacheMatrix and cacheSolve use scoping rules to store
## and retrieve a special matrix and compute, store, and retrieve its invse

## The function makeCacheMatrix defines and stores two functions, set to 
## store the matrix, and get to retrieve the matrix. The matrix is cached
## in the parent or global environment. 

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## The function cacheSolve retrieves the inverse of a special matrix,
## if it has been computed, or computes the inverse using the function
## solve it has not been previously computed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data)
        x$setinverse(mat)
        mat
}
