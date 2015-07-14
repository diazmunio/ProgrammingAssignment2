## Assignment 3 Lexical Scoping
## We have to create two functions, makeCacheMatrix to create a matrix 
## and cacheSolve to get the inverse of a matrix

## This function creates a Matrix object

makeCacheMatrix <- function(x = matrix()) {

        inv<- NULL
        
        set <-function(y) {
                x <<- y
                inv<<- NULL
        }
        
        get <- function() x
        
        setInverse<- function(solve) inv <<- solve
        
        getInverse <- function() inv
        
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function check if there's a result for the same matrix, 
## if not it returns the inverse of a matrix

cacheSolve <- function(x, ...) {
        
        inv<-x$getInverse()
        
        ## if there's an inverse matrix
        if (!is.null(inv)){
                message("Returns the cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(x,...)
        x$setInverse(inv)
        inv
}
