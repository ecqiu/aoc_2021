library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_6',colClasses='character',fill=T)

initial_lantern_timers=unlist(test)




fish_oneday=function(u){
  #filling in gaps
  for(i in 0:8){
    if( !(i %in% u$age)){
      u=rbindlist(list(u,data.table(age=i,N=0)))
    }
  }
  
  #age fish
  u[,age:=age-1]
  
  #count spawners
  n_spawn=u[age==-1,N]
  
  #set spawning fish counter to 6
  u=u[age>=0]
  u[age==6,N:=N+n_spawn]
  
  #add new fish at counter 8
  if(length(n_spawn)>0){
    u=rbindlist(list(u,data.table(age=8,N=0)))
    u[age==8,N:=N+n_spawn]
  }
  
  return(u)
}


#6.1
fishes=data.table(age=c(3,4,3,1,2))
fishes=data.table(age=as.numeric(initial_lantern_timers))
u=fishes[,.N,by=age]
for(i in 1:80){
  u=fish_oneday(u)
}
length(u)
u[,sum(N)]


#6.2
fishes=data.table(age=as.numeric(initial_lantern_timers))
u=fishes[,.N,by=age]
for(i in 1:256){
  u=fish_oneday(u)
}
length(u)
as.character(u[,sum(N)])


#terrible ideas graveyard

# spawn_fish=function(days,init_state){
#   if( paste0('p',init_state)%in% names(spawn_fish_mem)){return(spawn_fish_mem[[paste0('p',init_state)]])}
#   
#   day_vec=1:days -1 -init_state
#   spawn_days=which(day_vec %% 7==0 & day_vec>=0)
#   spawn_days=spawn_days
#   
#   
#   
#   
#   if(length(spawn_days)>0){
#     sub_spawns=unlist(lapply(spawn_days,function(x) spawn_fish(days,init_state=x+8)))
#     
#     spawn_fish_mem[[paste0('p',init_state)]]<<-c(spawn_days,sub_spawns)
#     return(c(spawn_days,sub_spawns))
#   }else{
#     spawn_fish_mem[[paste0('p',init_state)]]<<-list(NULL)
#     return(NULL)
#   }
# }


# fish_oneday=function(fishes){
#   out=fishes-1
#   out=c(out,rep(8,sum(fishes==0)))
#   out[out<0]=6
#   
#   return(out)
# }



#6.1
# spawn_fish_mem<<-list()

# fishes=data.table(timer=as.numeric(initial_lantern_timers))
# fishes=data.table(timer=c(3,4,3,1,2))
# test=microbenchmark(
#   fishes[,.N,by=timer][,.(N,unlist(lapply(timer,function(x) length(spawn_fish(80,x))) ))][,sum(N)+sum(N*V2)],times=10L
# )

# u=c(3,4,3,1,2)
# u=as.numeric(initial_lantern_timers)
# for(i in 1:80){
#   u=fish_oneday(u)
# }
# length(u)