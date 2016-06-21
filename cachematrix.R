## This pair of functions is used to cache the matrix and its inverse.

## makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
        x <<- y
        mat <<- NULL
        }
        get <- function() x
        
        set_inverse <- function(inverse) mat <<- inverse
        get_inverse <- function() mat
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve compute the inverse of the cache matrix from the above
## special 'vector' from its matrix through get function. But it will 
## skip if the inverse exist.

cacheSolve <- function(x, ...) {
        mat <- x$get_inverse()
        if(!is.null(mat)){
            message("getting cached data")
            return(mat)
        }
        data <- x$get()
        mat <- solve(x,...)
        x$set_inverse(mat)
        mat
}
