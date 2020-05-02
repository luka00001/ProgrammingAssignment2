## Caching the matrix inverse
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## Below function can be used to create a special object that stores a 
## matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	inversed_matrix<-NULL
	set<-function(y) {
		x<<-y
		inversed_matrix<<-NULL
	}
	
	get<-function() x
	setinverse<-function(inverse) inversed_matrix<<-inverse
	getinverse<-function() inversed_matrix
	list(set = set, get = get, 
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. Else calculate the inverse 
## and store it in cache and also returned calculated inverse of matrix. 

cacheSolve <- function(x, ...) {
        
		inversed_matrix<-x$getinverse()
		if(!is.null(inversed_matrix)){
			message("getting cached data")
			return(inversed_matrix)
		}
		
		data<-x$get()
		inversed_matrix<-solve(data, ...)
		x$setinverse(inversed_matrix)
		inversed_matrix
}
