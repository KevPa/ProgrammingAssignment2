## Because matrix inversion can be time costly when matrices are large,
## these functions will calculate the inversion only when the
## inverse has not already been computed.

## This function creates a matrix objects that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function calculates the inverse of the matrix.
## If the inverse has already been computed,
## it gets and returns that inverted matrix. 
## Otherwise it calculates the new inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Test
options(scipen=999) #Disable scientific notation
mat<-matrix(sample.int(100000,25^2,1),nrow=25,ncol=25)
cacheSolve(makeCacheMatrix(mat))


