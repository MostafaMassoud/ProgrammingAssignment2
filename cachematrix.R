## Put comments here that give an overall description of what your
## functions do

## Complete Example: 
##    > normalmat <- matrix(c(5,7,3,4,2,1,4,2,6), nrow=3, ncol=3)
##    > cachedmatfunc <- makeCacheMatrix(normalmat)
##    > cacheSolve(cachedmatfunc)
##      -- inverse of matrix
##    > cacheSolve(cachedmatfunc)
##     --inverse from cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        if (!is.matrix(x)) {
                stop("Please give a matrix")
        }
        
        inverted_matrix <- NULL
        set <- function(y) {
              x <<- y
              inverted_matrix <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) inverted_matrix <<- solve
        getinverse <- function() inverted_matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted_matrix <- x$getinverse()
        if(!is.null(inverted_matrix)) {
                message("getting cached data")
                return(inverted_matrix)
        }
        
        data <- x$get()       
        inverted_matrix <- solve(data, ...) 
        x$setinverse(inverted_matrix)	      
        inverted_matrix
}
