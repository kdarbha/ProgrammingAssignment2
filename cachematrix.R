## The Assignment 2 is to write a pair of functions that cache the inverse of a matrix.

## The below function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL ##Assigning a NULL to m
        
        set<-function(y){
                x<<-y 
                ## "<<-" will assign NULL to m defined in the makeCachematrix function
                m<<-NULL  
        }
        
        get<-function() {
                x
        }
        
        setmatrix<-function(solve){ 
                m<<- solve
        }
        
        getmatrix<-function(){
                m
        }
        ##creating and passing the list as the result
        list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
        
}


## The below function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getmatrix() ##getmatrix() will return inverse of the matrix if it exists
        
        ##Let us check if inverse exists and if it does exists let's return that
        if(!is.null(m)){
                
                message("getting cached data")
                return(m)
                
        }
        ## if we are here that means inverse is yet to be calculated 
        ## ok let's do that
        
        matrix<-x$get()
        
        ## we got the matrix through x$get() and now lets solve that 
        ## and return the inverse of it
        
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
