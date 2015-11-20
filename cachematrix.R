## Programming Assignment 2 for Coursea Class
## Two functions for dealing with matrix storage and calculation of inverse
## makeCacheMatrix recives a matrix as input and provides means to store the inverse
## cacheSolve - used for calculating inverse, also checks if invesre is already stored

## Usage:
## call makeCacheMatrix passing in a square matrix - assign the output to a variable
## ex:   z<- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## call cacheSolve against the output from makeCahceMatrix 
## ex: cacheSolve(z)
## z above was the variable we used for the output of MakeCacheMatrix
## Run cacheSolve again with the same variable - it will identify that the result is already cached

## Function that takes a marix and stores it allowing get, also allows for storing of the matrix inverse
## Assume we will get a square matrix as input
makeCacheMatrix <- function(x = matrix()) {
        
        #m stores the matrix, i stores the innverse - initially i is null
        m<<-x
        i<<-NULL
        
        #getter and setter functions
        set<-function(x){
                #If user is setting new value for the matrix 
                #then we need to clear the existing invesrse
                        m<<-x
                        i<<-NULL
                }
        
        get <- function() m
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        
        #List that is returned to the caller incudes the getter and setter functions
        list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)
}


## cahceSolve function - handles the calculation of inverse
## Designed to work with the makeCacheMatrix function (see usage info in earlier comments)
## assumes that the object passed in contains a function to set the inverse
## assumes the object passed in contains a function to check if the inverse was already stored "getinverse"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Check if x already had an inverse if it does show a message and return
        #If invesre has not been calculated yet, then x$getinverse will be null
        inv <- x$getinverse()
        
        if (!is.null(inv)){
                # inverse was not null, so we can get it from cache
                message("Invesre already in cache - showing stored inverse")
                
                #Return and exit
                return(inv)   
        }
        #Get the current matrix
        data<-x$get()
        
        #Calculate the Inverse
        inv <- solve(data) 
        
        #Set Inverse on x so it gets added to cache
        x$setinverse(inv)
        
        #Return the inverse 
        inv
}

#Function for simple testing
#Not part of assignment - but helps in checking that everything works
testcahceMatrix<-function(){
        message("Creating a matrix and storing to z - console will show z")
        z <- matrix(c(2,1,3,4,2,5,5,7,1,2,4,6,8,2,4,6),4,4)
        print(z)
        
        readline("Hit enter to continue")
        
        message("Using makeCachematrix to store z")
        mat <- makeCacheMatrix(z)
        message("calling get function from makeCacheMatrix - should return the original matrix")
        o <- mat$get()
        print(o)
        readline("Hit enter to continue")
        
        message("Checking to see if inverse is stored - we have not run solve yet so should be null")
        o<-mat$getinverse()
        print(o)
        readline("Hit enter to continue")
        
        message("running cacheSolve should store and display the inverse")
        o<-cacheSolve(mat)
        print(o)
        readline("Hit enter to continue")
        message("running cacheSolve again - this time it should be cached and should report as such")
        o<-cacheSolve(mat)
        print(o)
        
        readline("Hit enter to continue")
        message("Showing matrix times its inverse")
        m1<-mat$get()
        m2<-mat$getinverse()
        f <- m1 %*% m2
        print(f)
        readline("Hit enter to continue")
        
        message("setting new value for the matrix and calling get")
        z<-matrix(c(2,1,3,4),2,2)
        mat$set(z)
        o <- mat$get()
        print(o)
        readline("Hit enter to continue")
        
        message("Checking to see if inverse is stored - invesrse should have been set to null because matrix changed")
        o<-mat$getinverse()
        print(o)
        readline("Hit enter to continue")
        
        message("running cacheSolve should store and display the inverse")
        o<-cacheSolve(mat)
        print(o)
        readline("Hit enter to continue")
        message("running cacheSolve again - this time it should be cached and should report as such")
        o<-cacheSolve(mat)
        print(o)
        
        readline("Hit enter to continue")
        message("Showing matrix times its inverse")
        m1<-mat$get()
        m2<-mat$getinverse()
        f <- m1 %*% m2
        print(f)
}
