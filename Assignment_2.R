makeCacheMatrix <- function(x = matrix())
 {
    crr_inv <- NULL
    set <- function(y) {
        x <<- y
        crr_inv <<- NULL
    }
    get <- function()x
            
    setinverse <- function(inverse) 
    crr_inv <<- inverse
    getinverse <- function() 
    crr_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) # this function will compute inverse
 {
    crr_inv <- x$getinverse()
    if(!is.null(crr_inv)) {                #if matrix already exists in cache
        message("getting cached data.")
        return(crr_inv)
    }
    input_data <- x$get()               
    crr_inv <- solve(input_data) # solve is inbuilt function for inversion
    x$setinverse(crr_inv)
    crr_inv
}

#If determinent of this matrix is "0" ,it will throw an 
#error," Lapsack routine dgesv :system is exactly singular : U[2,2] =0