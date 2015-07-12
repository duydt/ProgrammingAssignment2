makeCacheMatrix <- function(x = matrix()) {
	 ## equals to NULL to indicate that the matrix is inversed already
	 ##this, actually look like the data member of class in OOP paradigm 
        isInversed <- NULL
        ##set value for x also assume that the matrix is inversed 
	set <- function(b){
	  x <<- b
   	  isInversed <<- NULL
	}
	##return the value of x
	get <- function(){
		return (x)
	}	
	##change the value of inverse which means that indicate if current matrix - x is inversed or not
	setInverse <- function(inverse){
		isInversed <<- inverse
	}
	##get the value of isInversed
	getInverse <- function()
	{	
		return (isInversed)
	}
	##list all the members of current class = makeCacheMatrix
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
        ##used to check if the matrix has been calculated the inversation or not
        isInversed <- x$getInverse()
	##if the isInversed has been calculated
	if(!is.null(isInversed)){
		message("getting cached data")
		return (isInversed) 
	}
	##calculate inversed,
	##1. get normal matrix of x named nm 
	nm <- x$get()
	##calculate and save inversed of nm
	x$setInverse(solve(nm) %*% nm)
	return (x$getInverse())
}

test<-function(){
	x <- stats::rnorm(9)
	dim(x) <- c(3,3)
	##view the solve(x)
	x
	##Get the inverse matrix of x:
	solve(x) %*% x
	##test the cachesolve function
	y <- makeCacheMatrix(x)
	z<-cacheSolve(y)
	z
}
