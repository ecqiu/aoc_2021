library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_9',header=F)
heights=data.matrix((as.data.table(tstrsplit(test$V1,split=''))))

# heights=matrix(as.numeric(unlist(tstrsplit('21999432103987894921985678989287678967899899965678',split=''))),nrow=10,ncol=5)

#9.1
lowpoints=c()
lowpoints_mat=c()
for(r in 1:nrow(heights)){
  for(c in 1:ncol(heights)){
    
    
    
    row_adjacencies=c(r+1,r-1)
    col_adjacencies=c(c+1,c-1)
    
    row_adjacencies=row_adjacencies[row_adjacencies<=nrow(heights)&row_adjacencies>0]
    col_adjacencies=col_adjacencies[col_adjacencies<=ncol(heights)&col_adjacencies>0]
    
    
    adjacencies=rbind(
      cbind(r,col_adjacencies)
      ,cbind(row_adjacencies,c)
    )
    
    
    
    
    
    
    
    if( heights[r,c]<min( heights[adjacencies])){
      lowpoints=c(lowpoints,heights[r,c])
      lowpoints_mat=rbind(lowpoints_mat,cbind(r,c))
      
    }
  }
}
sum(lowpoints+1)

adj=function(coord,nrows,ncols){
  r=(coord-1)%%nrows+1
  c=(coord-1)%/%nrows+1
  
  row_adjacencies=c(r+1,r-1)
  col_adjacencies=c(c+1,c-1)
  
  row_adjacencies=row_adjacencies[row_adjacencies<=nrows&row_adjacencies>0]
  col_adjacencies=col_adjacencies[col_adjacencies<=ncols&col_adjacencies>0]
  
  
  adjacencies=rbind(
    cbind(r,col_adjacencies)
    ,cbind(row_adjacencies,c)
  )
  
  adjacencies=adjacencies[,1]+(adjacencies[,2]-1)*nrows
  
  return(adjacencies)
}




#9.2
basin_sizes=data.table(index=1:nrow(lowpoints_mat))
for(i in 1:nrow(lowpoints_mat)){
  print(i)
  # start_coord=lowpoints_mat[i,]
  start_coord=lowpoints_mat[i,1]+(lowpoints_mat[i,2]-1)*nrow(heights)
  
  basin_coords=c(start_coord)
  
  n_new_coords=-1
  while(n_new_coords!=0){
    n_old_basin_coords=length(basin_coords)
  
    adjacent_coords=unlist(lapply(basin_coords,function(x)adj(x,nrows=nrow(heights),ncols=ncol(heights))))
    
    adjacent_coords=adjacent_coords[adjacent_coords>0 & adjacent_coords<=(nrow(heights)*ncol(heights))]
    adjacent_coords=unique(adjacent_coords[heights[adjacent_coords]!=9])
    
    basin_coords=unique(c(basin_coords,adjacent_coords))
  
    n_new_coords=length(basin_coords)-n_old_basin_coords
  }
  basin_sizes[i,size:=length(basin_coords)]  
  
}

prod(basin_sizes[order(-size)][1:3]$size)
