## The first function, "makeCacheMatrix", creates a special "matrix", which is really a list containing a function to: (1) set the value of the matrix, (2) get the value of the matrix, (3) set the value of the inverse matrix and (4) get the value of the inverse matrix.

## "makeCacheMatrix" takes in a matrix, and generates a list.

makeCacheMatrix <- function(x = matrix()) {

invmat<-NULL

  set<-function(y){

  x<<-y

  invmat<<-NULL

	}

get<-function() {x}

setinvmatrix<-function(inv1) {invmat<<- inv1}

getinvmatrix<-function() {invmat}

list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)

}

## "cacheSolve" finds the inverse of a matrix by: either, checking whether the inverse exists in a different environment from the one it runs in, and if so, printing "getting cached data" and the "cached" inverse of the matrix; or, (2) calculating the inverse of the matrix and priting it, and setting it as the inverse of the matrix in the "setinvmatrix". 

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

invmat<-x$getinvmatrix()

    if(!is.null(invmat)){

      message("getting cached data")

      return(invmat)

    }

    matrix<-x$get()

    invmat<-solve(matrix, ...)

    x$setinvmatrix(invmat)

    invmat

}

## E.g. of input and output: (1) create matrix "L" (> L<-matrix(c(1:4),2,2); (2) copy and paste "makeCacheMatrix" into your R-console, and run "L" through "makeCacheMatrix" to create "Lc" (> Lc<-makeCacheMatrix(L)); (3) copy and paste "cacheSolve" into your R-console, and run "Lc" through "cacheSolve" (> cacheSolve(Lc)); the output will be a matrix, where row 1 is "[-2 1.5]" and row 2 is "[1 -0.5]".