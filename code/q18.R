library(data.table)
lines=readLines(con = '/home/eqiu/code_projects/aoc_2021/data/input_18', n = -1L, ok = TRUE, warn = TRUE)

find_split_comma=function(pair){
  
  bracket_val=0
  
  for(i in 2:nchar(pair)){
    if(bracket_val==0&substr(pair,i,i)==','){
      return(i)
    }
    
    if(substr(pair,i,i)=='['){
      bracket_val=bracket_val+1
    }
    if(substr(pair,i,i)==']'){
      bracket_val=bracket_val-1
    }
  }
}

# find_split_comma(lines[2])
# substr(lines[2],2,14)

print_snail=function(list){
  
  if(is.numeric(list)){
    return(as.character(list))
  }
  
  out=paste0('[',print_snail(list[[1]]),',',print_snail(list[[2]]),']')
  return(out)
}

read_snail=function(pair){
  if(!grepl('[^0-9]',pair)){
    return(as.numeric(pair))
  }
  split_comma_i=find_split_comma(pair)
  
  
  out=list()
  out[[1]]=read_snail(substr(pair,2,split_comma_i-1) )
  # print(substr(pair,2,split_comma_i-1))
  out[[2]]=read_snail(substr(pair,split_comma_i+1,nchar(pair)-1))
  return(out)
}
# temp=read_snail(lines[2])
# str(temp)

leaf_list=function(in_list){
  index_list=list()
  
  for(i in 1:length(in_list)){
    if(is.numeric(in_list[[i]])){
      index_list[[i]]=c(i)
    }else{
      index_list[[i]]=lapply(leaf_list(in_list[[i]]),function(x) c(i,x))
    }
  }
  
  #flatten
  index_list=lapply(rapply(index_list, enquote, how="unlist"), eval)
  return(index_list)
}
# leaf_nodes=leaf_list(temp)


add_snail=function(x1,x2){
  out_list=list()
  out_list[[1]]=x1
  out_list[[2]]=x2
  
  # print(print_snail(out_list))
  #list of nodes to explode
  leaf_nodes=leaf_list(out_list)
  lp_nodes=lapply(leaf_nodes,function(x) x[-length(x)])
  lp_nodes=lp_nodes[which(duplicated(lp_nodes))]
  lp_nodes=unique(lp_nodes)
  to_explode=lp_nodes[lapply(lp_nodes,length)>=4]
  
  leafs=lapply(leaf_nodes,function(x)out_list[[x]])
  to_split=leaf_nodes[which(unlist(leafs>=10))]
  
  
  
  while(length(to_explode)>0|length(to_split)>0){
    # print(i)
    # print(lp_nodes)
    # print(to_explode[[1]])
    # print(length(to_explode))
    # print(length(to_split))
    if(length(to_explode)>0){
      exp_i=to_explode[[1]]
      
      # print(out_list[[c(exp_i)]])
      
      val_1=out_list[[c(exp_i,1)]]
      val_2=out_list[[c(exp_i,2)]]
      
      out_list[[exp_i]]=0
      
      left_index=which(unlist(lapply(leaf_nodes,function(x) length(x)==length(c(exp_i,1))&&all(x==c(exp_i,1))) ))
      right_index=left_index+1
      
      if(left_index>1){
        next_left=leaf_nodes[[left_index-1]]
        out_list[[next_left]]=out_list[[next_left]]+val_1
        # if(out_list[[next_left]]>=10){
        #   out_list[[next_left]]=list(floor(out_list[[next_left]]/2),ceiling(out_list[[next_left]]/2))
        # }
      }
      if(right_index<length(leaf_nodes)){
        next_right=leaf_nodes[[right_index+1]]
        out_list[[next_right]]=out_list[[next_right]]+val_2
        
        # if(out_list[[next_right]]>=10){
        #   out_list[[next_right]]=list(floor(out_list[[next_right]]/2),ceiling(out_list[[next_right]]/2))
        # }
        
      }
    }else{
      # print(print_snail(out_list))
      leaf_nodes=leaf_list(out_list)
      leafs=lapply(leaf_nodes,function(x)out_list[[x]])
      to_split=leaf_nodes[which(unlist(leafs>=10))]
      if(length(to_split)>0){
        out_list[[to_split[[1]]]]=list(floor(out_list[[to_split[[1]]]]/2),ceiling(out_list[[to_split[[1]]]]/2))
      }
    }
  
    # print(out_list)
    #list of nodes to explode
    leaf_nodes=leaf_list(out_list)
    lp_nodes=lapply(leaf_nodes,function(x) x[-length(x)])
    lp_nodes=lp_nodes[which(duplicated(lp_nodes))]
    lp_nodes=unique(lp_nodes)
    to_explode=lp_nodes[lapply(lp_nodes,length)>=4]
    leafs=lapply(leaf_nodes,function(x)out_list[[x]])
    to_split=leaf_nodes[which(unlist(leafs>=10))]
    # print(to_explode)
    # print(print_snail(out_list))
  }
  
  return(out_list)
}

s_mag=function(x){
  if(is.numeric(x)){
    return(x)
  }
  
  return(3*s_mag(x[[1]])+2*s_mag(x[[2]]))
}


# s1='[[[[4,3],4],4],[7,[[8,4],9]]]'
# s2='[1,1]'
# s1x=read_snail(s1)
# s2x=read_snail(s2)

# test=add_snail(s1x,s2x)

# lines=c(
#   '[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]',
#   '[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]',
#   '[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]',
#   '[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]',
#   '[7,[5,[[3,8],[1,4]]]]',
#   '[[2,[2,2]],[8,[8,1]]]',
#   '[2,9]',
#   '[1,[[[9,3],9],[[9,0],[0,7]]]]',
#   '[[[5,[7,4]],7],1]',
#   '[[[[4,2],2],6],[8,7]]'
# )

# lines=c(
#   '[1,1]',
#   '[2,2]',
#   '[3,3]',
#   '[4,4]',
#   '[5,5]',
#   '[6,6]'
# )


init=read_snail(lines[1])
out=init
for(i in 2:length(lines)){
  print(i)
  out=add_snail(out,read_snail(lines[i]))
  # print(out)
  # print(leaf_list(out))
}
test=s_mag(out)


#
max_val=0
for(i in 1:length(lines)){
  # print(i)
  for(j in 1:length(lines)){
    print(paste0(i,'_',j))
    if(i==j){
      next
    }
    out=s_mag(add_snail(read_snail(lines[j]),read_snail(lines[i])))
    print(out)
    max_val=max(max_val,out)
  }
  # print(out)
  # print(leaf_list(out))
}

