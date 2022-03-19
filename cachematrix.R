## makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.
##cacheSolve: This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##cacheSolve should retrieve the inverse from the cache.
## # creating a makeCacheMatrix with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                               # initialize inv as NULL; will hold value of matrix inverse
set <- function(y) {                     #  define the set function
x <<- y
inv <<- NULL
}
get <- function(){
x                                     # define the get fucntion - returns value of the matrix
}
setinverse <- function(inverse){
inv <<- inverse                       # assigns value of inv
}
getinverse <- function(){
inv                                    # gets the value of inverse- where it's called
}
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Here it computes the inverse of the "matrix" returned by makeCacheMatrix above.
## And cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
if(!is.null(inv)) {                                        #not null values
if ( identical( x$get() %*% inv , inv %*% x$get() ) ){ 
message("getting cached  matrix")
return(inv)                                       #returns a cached matrix inverse,already computed
}
}
data <- x$get()
inv <- solve(data)        #computes inverse of matrix
x$setinverse(inv)
inv                   # returns the inverse of a matrix 'X'

}

##TEsting
##> f1<-makeCacheMatrix(matrix(1:4,2,2))
##> f1$get()
  ##   [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(f1)
  ##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(f1)
##getting cached  matrix
  ##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> data
  ##   [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##x$get() %*% inv --->Identity matrix(inverse property)
  ##   [,1] [,2]
##[1,]    1    0
##[2,]    0    1
##> identical(f1$get() %*% inv,inv %*% f1$get())
##[1] TRUE