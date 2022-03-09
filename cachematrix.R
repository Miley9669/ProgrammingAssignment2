#cachematrix.R description:

##My two functions together will return the inverse of a matrix. In the process, it will cache the inverse for 
#a matrix the first time it's calculated. The 2nd function will pull the cached inverse matrix if it exists instead of
#calculating it a 2nd time.





#makeCacheMatrix() description

#Using an input of an invertible matrix. This function will return a list that contains 4 functions. It will
#also store objects x and i in its environment. The point of the function is to be used within the cacheSolve() function
#below. It stores the inverted matrix i which cacheSolve() will pull directly instead of recalculating it

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) { #set(): allows for resetting of input matrix for a specific object
    x<<- y
    i<<- NULL
  }
  get<-function() x #returns the value of the input matrix x
  setinverse<-function(inverse) i <<- inverse #takes input "inverse" and sets the object "i" in its parent environment
                                              #equal to "inverse"
  getinverse<-function() i #returns the object i that's in the function's parent environment
  list(set=set, get=get,  #Creates a list that contains the 4 functions defined above
       setinverse=setinverse,
       getinverse=getinverse)
}

#cacheSolve() description

## This function will take a list created by the makeCacheMatrix() function and will return the inverse of the matrix
#that was input into makeCacheMatrix()

cacheSolve <- function(x, ...) {
  i<-x$getinverse() #Search the input object to see if it has an inverse matrix, i, stored 
  if(!is.null(i)) { #Test to see if the object exists, i.e., doesn't return NULL
    message("getting cached data") #Let it be known that the inverse is already stored
    return(i) #return the inverse matrix
  }  #Lines will run if there was no object i stored in the object x, the input to cacheSolve()
  data<-x$get() #calls get() function w/in x. Returns the matrix that was the input to makeCacheMatrix()
  i<-solve(data, ...) #creates an object i in cacheSolve's environment which is the inverse of the matrix data
  x$setinverse(i) #calls function setinverse() which takes object i from the line above and 
    #sets i w/in the input object, x, equal to i w/in cacheSolve()
  i #returns the inverse matrix
}

#Test case

x<-matrix(c(2,4,5,6), nrow=2, ncol=2)
solve(x)
my_matrix<-makeCacheMatrix(x)
cacheSolve(my_matrix)  #returns same as solve(x)
cacheSolve(my_matrix)  #Returns inverse matrix but with "getting cached data" message





