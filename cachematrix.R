##input: matrix x
##output: class named makeCacheMatrix which contains information of matrix and matrix inversion
makeCacheMatrix <- function(x = matrix()) {
	 ## equals to NULL to indicate that the matrix is inversed already
	 ##this, actually look like the data member of class in OOP paradigm
	 ##isInversed = NULL if x is inversed otherwisre isInVersed = solve(x)
        isInversed <- NULL
        ##set value for x also assume that the matrix is inversed 
        ## they are look like member fucntions of class
	set <- function(b){
	  x <<- b
   	  isInversed <<- NULL
	}
	##return the value of x
	get <- function(){
		return (x)
	}	
	##change the value of matrix inversion which means that assign the value of matrix inversion named inverse to isInversed
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

## the below function is used to calculate the inverse of instance of makeCacheMatrix named x
##input: an instance of makeCacheMatrix named x
##output: matrix inversion of x
cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        ##used to check if the matrix has been calculated the Inversion or not
        ##get value of isInversed of x
        isInversed <- x$getInverse()
	##if the isInversed had been calculated -> no need to calculate matrix inversion anymore
	if(!is.null(isInversed)){
		message("getting cached data")
		return (isInversed) ##stop the calculation and return the output 
	}
	##calculate inverse of matrix
	##1. get matrix of x named nm 
	nm <- x$get()
	##2. calculate and save inversed of nm to x by using built-in function named solve
	x$setInverse(solve(nm,...))
	##3. return the matrix inversion of nm
	return (x$getInverse())
}
##this function is used to test and see how is makeCacheMatrix work only?
##input: nothing
##output: the output of matrix inversion and compare the our output with solve(x) - R built-in function. The matrix as the input 
##for cacheSolve is generated randomly 
test  <- function(){
     ##a square matrix with 4 rows and 4 columns
	x<-rbind(c(1,3,2,1),c(0,1,-1,-1),c(0,0,1,3),c(0,0,0,1))
	##Get the inverse matrix of x, save the result on t (using solve(x) - built-in function)
	t <- solve(x)
	##test makeCacheMatrix and the cachesolve function
	##1. make an instnace of makeCacheMatrix y from x
	y <- makeCacheMatrix(x)
	##2. calculate inverse of y, save the result on z
	z <- cacheSolve(y)
	##3. View the ouput of matrix inversion 
	z
	##check and compare values of z and t, it should return a square matrix with all items are TRUE
	##t == z
	##uncomments to check how does cached work?
	##try to calculate matrix inversion of x again -> expect to receive getting cached data message coz the matrix is inversed already
	##t <- cacheSolve(y)
	##view values of t
	##t
}
