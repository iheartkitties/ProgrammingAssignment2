## makeCacheMatrix functions as a storage for user's original matrix and its inverse. 
## cacheSolve will either display already stored inverse or will calulcate the inverse of the matrix if inverse is not stored. After calulcation, the matrix and its inverse will be stored.

#makeCacheMatrix function: 
#1. Set: setMatrix x by entering a list of numbers. The matrix function will return a matrix. 
#2. Reall: display Matrix x when called. 
#3. Setinv: solving inverse of matrix. 
#4. Getinv: display inverse of matrix when called. 
#5. list function consolidates function 1-4 within function makeCacheMatrix. 
makeCacheMatrix <- function(x=matrix() ) {
        s<-NULL
        set <- function(y) {
                x <<- matrix(y, nrow=sqrt(length(y)), byrow=TRUE)
                s <<- NULL
        }
        recall <- function() x
        setinv <- function(x){
        		x=matrix(x, nrow=sqrt(length(x)), byrow=TRUE)
        		s<<-solve(x)
        }
        getinv <- function() s        
        list(set=set, recall=recall, setinv = setinv, getinv = getinv)
}

#cacheSolve function: 
#Get matrix's inverse from getinv from makeCacheMatrix function. 
#if getinv returns "NULL" then function will caluculate the matrix's inverse. 
#if getinv does not yield "NULL" then function will call getinv and display "getting cached data" and show inverse. 

cacheSolve <- function(x,...) {
        s<-x$getinv()
        if(is.null(s)){
            data<- x$recall()
        	s <- solve(matrix(data, nrow=sqrt(length(data)), byrow=TRUE))
        	x$setinv(data)
       		return(s)  
        }else{        
             message("getting cached data")
             x$getinv()
        }        
}