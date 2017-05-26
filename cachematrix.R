################################################################################
######This function creates a special "matrix" object that can cache its inverse.
##This function creates a default matrix defined as x 
makeCacheMatrix <- function (x=matrix()) {
        
        
##create a undefined Variable inv_matrix with NULL
##that will hold the value of the inversed matrix
        inv_matrix <- NULL
        
##define the set function to assine new (with y)
        set <- function(y){
                
##the new assignment matrix defined back to the 
##initial matrix variable in the parent environment 
                x <<- y
                
##if there is a new assigned matrix, reset inv_matrix to NULL 
                inv_matrix <<- NULL
        }
        
##define the get function 
##it returns the value of the matrix argument
        get <- function()x
        
##assigns value of inv_matrix
        setinverse <- function(solve) inv_matrix <<- solve
        
##get the value of inv_matrix
        getinverse <- function() inv_matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


################################################################################
####This function computes the inverse of the special "matrix" returned by 
####makeCacheMatrix above. If the inverse has already been calculated 
####(and the matrix has not changed), then the cachesolve should retrieve the 
####inverse from the cache.
##the cacheSolve function retrieve the matrix from the cache
cacheSolve <- function(x,...){
        
        
##the inv_matrix gets its former defined value
        inv_matrix <- x$getinverse()
        
##The if loop checks whether the matrix is defined or undefined
        if(!is.null(inv_matrix)){
                
## if it's defined it prints out "getting cached data
                print("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        
##the solve function creates the inverse matrix
        inv_matrix<- solve(data,...)
        x$setinverse(inv_matrix)
        inv_matrix
}
