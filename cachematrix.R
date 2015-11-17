## Programming Assignment 2 for Corsea Class
## Put comments here that give an overall description of what your
## functions do

## Function that takes a marix and stores it allowing get, also allows for storing of the matrix inverse
## Assume we will get a square matrix as input

makeCacheMatrix <- function(x = matrix()) {

        #Variables used by code intrnally
        m<<-x
        i<<-NULL
        
        #No setter function - only get
        get <- function() m
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        
        #Last that is returned to the caller
        list(get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Check if x already had an inverse if it does show a message and return
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                
                #Return and exit
                return(inv)   
        }
        #Get the current matrix
        data<-x$get()
        
        #Get Inverse
        inv <- solve(data) 
        #Set Inverse on x so it gets added to cache
        x$setinverse(inv)
        #Return the inverse as well
        inv
}
