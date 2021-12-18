library(data.table)

grid=fread('/home/eqiu/code_projects/aoc_2021/data/input_15',header=F)

grid=as.matrix(as.data.table(tstrsplit(grid$V1,split='')))
storage.mode(grid)='numeric'

dists=matrix(Inf,nrow=100,ncol=100)

dists[100,100]=0



adjs=function(val,nrow,ncol){
  adjs=c()
  if(!((val-1) %/% nrow==0)){
    adjs=c(adjs,val-nrow)
  }
  if(!((val-1) %/% nrow==(ncol-1))){
    adjs=c(adjs,val+nrow)
  }
  if(!((val) %% nrow==1)){
    adjs=c(adjs,val-1)
  }
  if(!((val) %% nrow==0)){
    adjs=c(adjs,val+1)
  }
  
  return(adjs)
}
# adjs(val=10,nrow=10,ncol=10)


update_grid=function(dists,grid){
  to_check=sort(unique(c(unlist(lapply(which(is.finite(dists)),function(x) adjs(x,nrow(dists),ncol(dists)))),which(is.finite(dists)) ) ),decreasing=T)
  n_changed=c()
  
  while(length(to_check)>0){
    check_dists=dists[to_check]
    min_check_vals=unlist(lapply(to_check,function(x){a=adjs(x,nrow(dists),ncol(dists));min(grid[a]+dists[a]) }))
    change_vals=to_check[check_dists>min_check_vals]
    
    if(length(change_vals)>0){
      dists[change_vals]=min_check_vals[check_dists>min_check_vals]
      to_check=unique(unlist(lapply(change_vals,function(x)adjs(x,nrow(dists),ncol(dists)))))
    }else{
      to_check=c()
    }
    # print(to_check)
  }
  
  
  # while(length(to_check)>0){
  #   p=to_check[1]
  #   adjs=adjs(p,nrow(dists),ncol(dists))
  #   if(dists[p]>min(grid[adjs]+dists[adjs])){
  #     to_check=c(to_check,adjs) #can comment out for a slower alg that can be called on repeatedly to track progress
  #     n_changed=c(n_changed,p)
  #     dists[p]=min(dists[p],grid[adjs]+dists[adjs])
  #   }
  #   to_check=to_check[-1]
  #   to_check=unique(to_check)
  # }
  return(list(dists=dists,n_changed=n_changed))
}
#15.1
# n_changed=c(0)
# i=0
# while(length(n_changed)>0){
  out_list=update_grid(dists,grid=grid)
  n_changed=out_list$n_changed
  dists=out_list$dists
  # i=i+1
  # print(i)
# }


#15.2
grid2=cbind(grid,grid+1,grid+2,grid+3,grid+4)
grid3=rbind(grid2,grid2+1,grid2+2,grid2+3,grid2+4)
grid4=grid3%%9
grid4[grid4==0]=9

dists2=matrix(Inf,nrow=nrow(grid4),ncol=ncol(grid4))
dists2[nrow(grid4),ncol(grid4)]=0

# n_changed=c(0)
# i=0
# while(length(n_changed)>0){
  out_list=update_grid(dists=dists2,grid=grid4) 
  n_changed=out_list$n_changed
  dists2=out_list$dists
  # i=i+1
  # print(i)
# }