## cachematrix contains code related to inverse of matrix. This code generates
## matrix using function makeCacheMatrix function and gets inverse of matrix 
## using function cacheSolve which checks if inverse is already calculated or not
## if inverse is already calculated it returns cache value else it calculated inverse
## and retun inverse of given matrix.

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invr) inv <<- invr
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix created 
## with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("value already calculated.. printing from cache")
                return(inv)
        }
        data <- x$get()
        if(det(data)==0) # Checking if matrix is invertible or not
        {
                message("determinant of matrix is zero,matrix is not invertible")
                return(NULL)
        }
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
