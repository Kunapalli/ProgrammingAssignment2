## Example: Caching the inverse of a matrix
## In this program we use the <<- operator which can be used to 
## assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are used 
## to create a special object that stores a matrix and cache's its matrix.

## makeCacheMatrix takes in a matrix and returns a new kind of matrix, 
## what we call a cached matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(m2) m <<- m2
        getinverse <- function() m
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}



##The following function calculates the matrix of the special "matrix" created 
##with the above function. However, it first checks to see if the matrix has 
##already been calculated. If so, it gets the matrix from the cache and 
##skips the computation. Otherwise, it calculates the matrix of the data and 
## sets the value of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
