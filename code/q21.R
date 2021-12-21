library(data.table)
dice=fread('/home/eqiu/code_projects/aoc_2021/data/input_21',header=F)

#21.1
p1_pos=dice$V5[1]
p2_pos=dice$V5[2]

p1_score=0
p2_score=0

i=1
d_roll=0
while(p1_score<1000 & p2_score<1000){
  print('d')
  print(d_roll)
  scores=i:(i+2)
  d_roll=d_roll+3
  scores=scores%%100
  scores[scores==0]=100
  i=i+3
  p1_pos=(p1_pos+sum(scores))%%10
  if(p1_pos==0){
    p1_pos=10
  }
  p1_score=p1_score+p1_pos
  
  if(p1_score<1000){
    d_roll=d_roll+3
    scores=i:(i+2)
    scores=scores%%100
    scores[scores==0]=100
    
    i=i+3
    p2_pos=(p2_pos+sum(scores))%%10
    if(p2_pos==0){
      p2_pos=10
    }
    p2_score=p2_score+p2_pos
  }
  
  print(p1_pos)
  print(p2_pos)  
}


d_roll*p2_score


dt_map=data.table(i=1:27)
for(i in 1:3){for(j in 1:3){for(k in 1:3){
  ind=(i-1)*9+(j-1)*3+k
  val=sum(c(i,j,k))
  dt_map[ind,x:=val]
}}}
dt_counts=dt_map[,.N,by=x][order(-x)]


#21.2
return:n_perms

enumerate_player_outcomes=function(p1_pos,score=0){
  out_list=list()
  if(score>=21){
    # return(-1)
    # return(list(c(l=0,n_perms=1)))
    return(data.table(l=0,n_perms=1))
  }
  
  # temp_list=list()
  for(val in 3:9){
    temp_val=enumerate_player_outcomes((p1_pos+val-1)%%10+1,score+(p1_pos+val-1)%%10+1)
    # out_list[[val-2]]=lapply(temp_val,function(x) c(dt_counts[x==val]$N,x))
    # out_list[[val-2]]=lapply( temp_val, function(x) c(x[1]+1,x[2]*dt_counts[x==val]$N)) 
    temp_val=copy(temp_val)
    temp_val[,l:=l+1]
    temp_val[,n_perms:=n_perms*dt_counts[x==val]$N]
    out_list[[val-2]]=temp_val
  }
  
  # for(i in 1:3){for(j in 1:3){for(k in 1:3){
  #   rolls=c(i,j,k)
  #   val_lookup=sum(rolls)
  #   out_list[[(i-1)*9+(j-1)*3+k]]=lapply(temp_list[[val_lookup]],function(x) c(rolls,x))
  # }}}
  out_list=rbindlist(out_list)[,.(n_perms=sum(n_perms)),by=l]
  return(out_list)
}

p1_wins=enumerate_player_outcomes(dice$V5[1],0)
p2_wins=enumerate_player_outcomes(dice$V5[2],0)
# p1_wins=enumerate_player_outcomes(4,0)
# p2_wins=enumerate_player_outcomes(8,0)


p1_summ=copy(p1_wins)[order(l)]
p2_summ=copy(p2_wins)[order(l)]

n_yesx=0
for(i in 1:nrow(p1_summ)){
  n_yesx=n_yesx+p1_summ[i,]$n_perms
  p1_summ[i,n_yes:=n_yesx]
  n_yesx=n_yesx*3^3
}
p1_summ[,n_no:=3^(l*3)-n_yes]

n_yesx=0
for(i in 1:nrow(p2_summ)){
  n_yesx=n_yesx+p2_summ[i,]$n_perms
  p2_summ[i,n_yes:=n_yesx]
  n_yesx=n_yesx*3^3
}
p2_summ[,n_no:=3^(l*3)-n_yes]

p1_winst=0
p2_winst=0
for(i in 3:max(p1_summ$l,p2_summ$l)){
  if((i-1) %in% p1_summ$l){
    p1_losses=p2_summ[l==i-1]$n_no
  }else{
    p1_losses=27^(i-1)
  }
  
  if(i %in% p2_summ$l){
    p2_losses=p1_summ[l==i]$n_no
  }else{
    p2_losses=27^(i)
  }
  
  p1_winst=p1_winst+p1_summ[l==i]$n_perms*p1_losses
  p2_winst=p2_winst+p2_summ[l==i]$n_perms*p2_losses
  print(p1_winst)
}
as.character(p1_winst)
as.character(p2_winst)


