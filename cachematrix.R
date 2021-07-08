## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inversa <- NULL	##Initializing inverse as null for this section
	set<- function(y){
		x<<-y
		inversa<<-NULL
	}
	get<-function(){x}  ##This pint we can see function to get matrix x
	setInversa<-function(inversacal){inversa<<-inversacal}
	getInversa<-function() {inversa} ##Here we going to obtain inverse of the matrix

	list(set = set, get = get,
	     setInversa = setInversa,
	     getInversa = getInversa)
	    }

x<-makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)) ##Funtion to check  matrix x

x$get() ##Check the previous information


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  ##gets cache data
	inversa <-x$getInversa()
	if(!is.null(inversa)) {   ##checking whether inverse in NULL
		message("getting cached data")
		return(inversa)     ##return inverse value
	}
	data<-x$get()
	inversa <- solve(data, ...)  ##calculate inverse data
	x$setInversa(inversa)
	inversa   ## Return a matrix that is inverse of X
}

cacheSolve(x)
