library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_11',header=F,colClasses='character')


# grid_orig=matrix(as.numeric(strsplit('5483143223274585471152645561736141336146635738547841675246452176841721688288113448468485545283751526',split='')[[1]]),nrow=10,ncol=10)
grid_orig=as.matrix(as.data.table(tstrsplit(test$V1,split='')))
storage.mode(grid_orig)='numeric'

get_neighbs=function(p,nrows,ncols){
  c=(p-1)%/%ncols+1
  r=(p-1)%%ncols+1
  
  neighb_coords=rbind(
    expand.grid(r,c(c+1,c-1))
    ,expand.grid(c(r+1,r-1),c)
    ,expand.grid(c(r+1,r-1),c(c+1,c-1))
  )
  
  neighb_coords=neighb_coords[neighb_coords[,1]>0&neighb_coords[,1]<=nrows,]
  neighb_coords=neighb_coords[neighb_coords[,2]>0&neighb_coords[,2]<=ncols,]
  
  neighbs=neighb_coords[,1]+(neighb_coords[,2]-1)*nrows
  
  return(neighbs)
}

#11.1
n_steps=100
pops_dt=data.table(step=1:n_steps)
grid=grid_orig
for(i in 1:n_steps){
  print(i)
  new_grid=grid+1
  pops=which(new_grid==10)
  pop_queue=pops
  while(length(pop_queue)>0){
    adjs=get_neighbs(pop_queue[1],nrows=nrow(grid),ncols=ncol(grid))
    new_grid[adjs]=new_grid[adjs]+1
    
    pop_queue=c(pop_queue,adjs[new_grid[adjs]==10])
    pops=c(pops,adjs[new_grid[adjs]==10])
    pop_queue=pop_queue[-1]
  }
  pops_dt[i,`:=`(n_pops=length(pops))]
  new_grid[new_grid>=10]=0
  
  grid=new_grid
}
pops_dt[,sum(n_pops)]

#11.2

i=0
grid=grid_orig
pops=c()
while(length(pops) <length(grid_orig) ){
  i=i+1
  print(i)
  new_grid=grid+1
  pops=which(new_grid==10)
  pop_queue=pops
  while(length(pop_queue)>0){
    adjs=get_neighbs(pop_queue[1],nrows=nrow(grid),ncols=ncol(grid))
    new_grid[adjs]=new_grid[adjs]+1
    
    pop_queue=c(pop_queue,adjs[new_grid[adjs]==10])
    pops=c(pops,adjs[new_grid[adjs]==10])
    pop_queue=pop_queue[-1]
  }
  # pops_dt[i,`:=`(n_pops=length(pops))]
  new_grid[new_grid>=10]=0
  
  grid=new_grid
}
# pops_dt[,sum(n_pops)]