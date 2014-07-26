

## makeCacheMatrix function, This function used to create a special "matrix" object that 
#  can cache its inverse. It returns a list containing a function to
# setMatrix :- set the value of the matrix
# getMatrix :- get the value of the matrix
# setInvMatrix :- set the value of the inverse matrix
# getInvMatrix :- get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
   InvMat <- NULL  # Initialize witn NULL Inverse
   
   setMatrix <- function(y){
          x <<- y           #Initialize with new value of matrix
          InvMat <<- NULL   #Initialize with NULL Inverse when the matrix changed
   }
   getMatrix <- function() x  #To return current matrix
   setInvMatrix <- function(newInvMat) InvMat<<-newInvMat  # Set new Inverse matrix
   getInvMatrix <- function() InvMat  # Return the value of Inverse Maatrix
   list(setMatrix=setMatrix,getMatrix=getMatrix,setInvMatrix=setInvMatrix
        ,getInvMatrix=getInvMatrix)  # To return the list of function to create special "matrix" that can cache its inverse
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInvMatrix()  # Get current inverse
  if(!is.null(invMat)) {         # To check inverse is already computed
        message("getting cached inverse matrix")
        return(invMat)   # retrun Inverse matrix
  }
  Mat <- x$getMatrix()  # Get current matrix to local
  InvMat <- solve(Mat) # compute the inverse
  x$setInvMatrix(InvMat) # Initialize the inverse matrix using the function
  InvMat # Return the inverse matrix
}

# To test the 
#x<-makeCacheMatrix()
#a<-matrix(rnorm(16),4,4)
#x$setMatrix(a)
#cacheSolve(x)
#cacheSolve(x) #second call to check cached inverse matrix returned
