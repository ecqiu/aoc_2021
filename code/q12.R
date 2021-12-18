library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_12',header=F,colClasses='character')


# test=data.table(V1=
# c(
#   'dc-end',
#   'HN-start',
#   'start-kj',
#   'dc-start',
#   'dc-HN',
#   'LN-dc',
#   'HN-end',
#   'kj-sa',
#   'kj-HN',
#   'kj-dc'
# )
# )


# test=data.table(V1=
#   c(
#     'start-A',
#     'start-b',
#     'A-c',
#     'A-b',
#     'b-d',
#     'A-end',
#     'b-end'
#   )
# )

test[,`:=`(
  from = gsub('(.*)-(.*)','\\1',V1),
  to = gsub('(.*)-(.*)','\\2',V1  )
)]


test2=test[,.(from=to,to=from)]

paths=rbindlist(list(
  test[,.(from,to)],
  test2[,.(from,to)]
)
)

# paths

find_p=function(path_so_far=c('start'),mode='q1'){
  node=path_so_far[length(path_so_far)]
  
  if(node=='end'){
    return(list(path_so_far))
  }
  
  next_nodes=paths[from==node]$to
  
  if(
    mode=='q2'&
    sum(duplicated(grep('([a-z]+)',path_so_far,value=T)))==0 ){
    banned_nodes=c('start')
  }else{
    banned_nodes=grep('(start|[a-z]+)',path_so_far,value=T)
  }
  
  
  next_nodes=setdiff(next_nodes,banned_nodes)
  if(length(next_nodes)==0){
    return(NULL)
  }
  
  new_paths=lapply(1:length(next_nodes),function(x)c(path_so_far,next_nodes[x]))
  
  
  return(lapply(new_paths,function(x)find_p(path_so_far=x,mode=mode)))
}

paths_found=find_p()
paths_neat=lapply(rapply(paths_found, enquote, how="unlist"), eval)

#part2 solution a bit slow (maybe ~5min)
paths_found2=find_p(mode='q2')
paths_neat2=lapply(rapply(paths_found2, enquote, how="unlist"), eval)
