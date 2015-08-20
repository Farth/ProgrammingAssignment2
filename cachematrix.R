## Put comments here that give an overall description of what your
## functions do
## 
## Two functions are used to implement a cache system for solving
## a matrix. The first function makeCacheMatrix is used to create
## an object in order to add methods for a matrix.
## The second one cacheSolve is used to make the process itself.
## So you have to use the makeCacheMatrix on the matrix you want
## solve and then use the cacheSolve to solve and cache the result.


## Write a short comment describing this function
## This function takes a matrix in input and set 4 methods
## for the object 'm' : 
## - set : for setting a matrix to the object
## - get : to get back and print the matrix
## - setMatrix : method for solving the matrix
## - getMatrix : method for getting back the solved matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(m) m <<- solve(x)
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## Write a short comment describing this function
## This function takes in input an object given by the makeCacheMatrix
## function. It tests if the solved matrix is in the cache and,
## if not, it solves the matrix and cache the result in the object
## before printing the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
