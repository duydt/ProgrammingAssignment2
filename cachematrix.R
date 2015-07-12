## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        isInversed <- NULL
	set <- function(b){
	  a <<- b
   	  isInversed <<- NULL
	}
	get <- function(){
	return (a)
	}	
      setInverse <- function(inverse){isInversed <<- inverse}	
	getInverse <- function()
	{	
		return (isInversed)
	}
	list (
	set = set, 
	get = get,
	setInverse = setInverse, 
	getInverse = getInverse
	) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	isInversed <- x$getInverse()
	##if the isInversed has been calculated
	if(!is.null(isInversed)){
		message("getting cached data")
		isInversed 
	}
	##calculate inversed, but first get normal matrix name nm 
	nm <- x$get()
	##calculate and save inversed of nm
	x$setInverse(solve(nm) %*% nm)
	return (nm)
}
