## Put comments here that give an overall description of what your
## functions do:
# makeCacheMatrix is able to create and evaluate an square matrix and create a list containing it and its inverse.
# cacheSolve looks for the inverse of the given square matrix, and calculates it if there is not one yet.


## Write a short comment describing this function:
# makeCacheMatrix creates an object (a list) where te matrix and its inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y){
                x <<-y
                inv <<-NULL
        }
        getmatrix <- function() x
        setinverse <- function(solvedmatrix) inv <<- solvedmatrix
        getinverse <- function() inv
        list(setmatrix = setmatrix, 
             getmatrix=getmatrix, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## Write a short comment describing this function:
# cacheSolve looks for the cache within the matrix to see if there is already an inverse matrix, it there is not,
#it calculates it and caches it, so it can return it at the end

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message('getting cached data')
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}