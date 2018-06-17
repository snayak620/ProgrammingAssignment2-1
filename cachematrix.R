## makeCacheMatrix is a funciton that will create a matrix that can cache its inverse
## cachSolve is a function that will compute the inverse through the previous function, otherwise it will retrieve the inverse 

makeCacheMatrix <- function(x=matrix())  ## creates and computers an object matrix which can find/cache its inverse
	inversematrix <- NULL
	set <- function(y){
		x <<- y  
		inversematrix <<- NULL 
		}
	get <- function()x
	setinverse <- function(inverse) inversematrix <<- inverse
	setinverse <- function() inversematrix
	list (set = set, get = get,
		setinverse = setinverse, 
		getinverse= getinverse) 
		}
cacheSolve <- function (x,...){ ##computes the inverse, otherwise retrieves the inverse from the cache 
	inversematrix <- x$getinverse()
	if($is.nULL(inversematrix){
		message ("finding cached data")
		return (inversematrix) 
	}
	datam <- x$get() 
	inversematrix <- solve (datam)
	x$setinverse(inversematrix)
	inversematrix
}
## testing
x = rbind(c(1,2), c(3,4))
m = makeCacheMatrix(x)
m$get() 
 
		


		