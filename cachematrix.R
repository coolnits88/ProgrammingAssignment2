## cachematrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.
## Stores inverse of matrix if calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse<- function() i
        list(set = set, get = get,
             setinverse = setInverse,
             getinverse = getInverse)
}


## Return the inverse of an cacheMatrix object
## It checks if the inverse has been calculated already and by checking object returned by getInverse method.
## If not null returned object returned else calculate inverse and store it in special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
