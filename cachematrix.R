## Programming Assignment 2 for Corsea Class
## Two functions for dealing with matrix storage and calculation of inverse
## makeCacheMatrix recives a matrix as input and provides means to store the ionverse as well
##CacheSolve - used for calculating inverse, also checks if invesre is already stored

## Usage:
## call makeCacheMatrix passing in a aquare matrix - assign the output to a variable
## ex:   z<- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## call cacheSolve against the output from makeCahceMatrix 
## ex: cacheSolve(z)
## z above was the variable we used for the output of MakeCacheMatrix
## Run cacheSolve again with the same variable - it will identify that the result is already cached

## Function that takes a marix and stores it allowing get, also allows for storing of the matrix inverse
## Assume we will get a square matrix as input
makeCacheMatrix <- function(x = matrix()) {
        #x shuuld be a square matrix
        if(ncol(x)!=nrow(x)){
                stop("You must pass a square matrix to this function")
        }

        #Variables used by code internally
        #m stores the matrix, i stores the innverse - initially i is null
        m<<-x
        i<<-NULL
        
        #No setter function needed - only get
        get <- function() m
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        
        #List that is returned to the caller incudes the getter and setter functions
        list(get=get,setinverse=setinverse,getinverse=getinverse)
}


## cahceSolve function - handles the calculation of inverse from teh matrix
## Designed to work with the makeCacheMatrix function
## assumes that the object passed in contains a function to set the inverse
## assumes the object passed in contains a function to check if the inverse was already stored "getinverse"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Check if x already had an inverse if it does show a message and return
        #If invesre has not been calculated yet, then x$getinverse will be null
        inv <- x$getinverse()
        
        if (!is.null(inv)){
                # inverse was not null, so we can get it from cache
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
        
        #Return the inverse 
        inv
}
