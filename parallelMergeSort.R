library("multicore")

mergeChildren=function(matrixLR){
  #Left child
  left=matrixLR[1,]
  #Right child
  right=matrixLR[2,]

  lenLeft=length(left)
  lenRight=length(right)

  count=1
  merge=c()
  for(i in 1:(lenLeft+lenRight)){
    # if first element of left is NA then left elements have exhausted.
    #Right elements must be appended to list. Similar for right child.

    if(is.na(left[1])){
      merge<-append(merge,right)
      break
    }else if(is.na(right[1])){
      merge<-append(merge,left)
      break
    }

    if(left[1] <= right[1]) {
      #if the first element of left child is less, it goes to merge list
      #and it is removed from left child. It works similarly for right child.

      merge[count]=left[1]
      if(length(left)>1){
        left=left[2:length(left)]
      }else{
        merge<-append(merge,right)
        break;
      }
      count=count+1
    }else{
      merge[count]=right[1]
      if(length(right)>1){
        right=right[2:length(right)]
      }else{
        merge<-append(merge,left)
        break;
      }
      count=count+1
    }
  }
  return(merge)
}


#Main function. Formats data appropriately at each depth and makes call to
#mergeChildren which does merging parallely.

mergeSort=function(dataVector){
  dataList=as.list(dataVector)  
  
  #This loops runs for approx. log(length(dataVector)) times. 
  #One time for each depth
  while(length(dataList)>1){
      listElement=as.list(c())

      #This loops runs half the total number of children.
      #Initially all children are of length 1.

      for(p in 1:ceiling(length(dataList)/2)){
        #Left the odd indexed child. Right child is even indexed.

        leftElementLen=length(dataList[[2*p-1]])
        if(2*p<=length(dataList)){
          rightElementLen=length(dataList[[2*p]])
        }

        colCount=max(leftElementLen,rightElementLen)

        #childMatrix contains left and child elements in a tree.
        childMatrix=matrix(,2,colCount)

        #Matrix is populated with data and if smaller than no. of columns,
        #it is padded with NA.
        childMatrix[1,]=c(dataList[[2*p-1]],rep(NA,colCount-leftElementLen))

        if(2*p<=length(dataList)){
          childMatrix[2,]=c(dataList[[2*p]],rep(NA,colCount-rightElementLen))
        }

        #Add the childMatrix to list
        listElement[[p]]=childMatrix
    }

    dataList=as.list(c())
  
    #Parallel call to mergeChildren. Returns a list with left and right
    #children merged in sorted order

    dataList=mclapply(listElement,mergeChildren)  
  }

  result=unlist(dataList)

  #Remove all padded NA elements
  result<-result[!is.na(result)]

  return(result)
}

#Example Usage
#a=c(24,2,65,1,33,21,-1,68, 2)
#x=mergeSort(a)
