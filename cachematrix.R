## This R file contains 2 functions: makeCacheMatrix and cacheSolve

# makeCacheMatrix returns a special matrix object which is a list of 4 functions
## 1. set - set the matrix to argument value and inverse matrix to empty in cache
## 2. get - returns the matrix (if available)
## 3. setInvM - sets the inverse matrix in cache
## 4. getInvM - gets the inverse matrix from cache
makeCacheMatrix <- function(x = matrix()) {
  invMtrx<- NULL
  set<-function(m){
    mtrx<<-m 
    invMtrx<<-NULL
  }
  get<-function() x
  setInvM<-function(tInvMtrx) invMtrx<<-tInvMtrx
  getInvM<-function() invMtrx
  list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## cacheSolve will look for Imtrx in cache and return it if it is found
## if it is not found, it will solve the matrix and set it in cache

cacheSolve <- function(x, ...) {
  tInvMtrx<-x$getInvM()
  if(!is.null(tInvMtrx)){
    message("Returning cached inverse matrix.")
    return(tInvMtrx)
  }
  tmtrx<-x$get()
  tInvMtrx<-solve(tmtrx,...)
  x$setInvM(tInvMtrx)
  tInvMtrx
}

## Example for testing purposes
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
B<-makeCacheMatrix(A)
cacheSolve(B)
