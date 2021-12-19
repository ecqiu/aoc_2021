library(data.table)
area=fread('/home/eqiu/code_projects/aoc_2021/data/input_17',header=F)

x1=as.numeric(gsub('target area: x=(\\d+).*','\\1',area$V1))
x2=as.numeric(gsub('target area: x=\\d+\\.{2}(\\d+)','\\1',area$V1))

y1=as.numeric(gsub('y=(-\\d+).*','\\1',area$V2)) #lazy hardcoded minus
y2=as.numeric(gsub('y=-\\d+\\.{2}(-\\d+)','\\1',area$V2)) #lazy hardcoded minus


time=1:1000#hacky, just made it large enough to cover




##hacking it up
#17.1

zone=c()
init_x=0
while(length(zone)==0){
  init_x=init_x+1
  x_pos=ifelse(time<=init_x,time*init_x - time*(time-1)/2, init_x*init_x - init_x*(init_x-1)/2)
  zone=which(x_pos>=x1&x_pos<=x2)
}


# init_x
# init_x

plausible_y=0
init_y=0

for(init_y in 0:1000){
  y_pos=time*init_y-time*(time-1)/2
  if(any(y_pos[zone]>=y1 &y_pos[zone]<=y2)){
    plausible_y=init_y
  }
}

init_y=116
y_pos=time*init_y-time*(time-1)/2
max(y_pos)


#17.2
can_list=list()
list_i=1
for(init_x in 18:x2){
  for(init_y in y1:1000){#1000 is hacky, should work out max y such that forced to overshoot 
    x_pos=ifelse(time<=init_x,time*init_x - time*(time-1)/2, init_x*init_x - init_x*(init_x-1)/2)
    zone=which(x_pos>=x1&x_pos<=x2)
    y_pos=time*init_y-time*(time-1)/2
    if(any(y_pos[zone]>=y1 &y_pos[zone]<=y2)){
      can_list[[list_i]]=c(init_x,init_y)
      list_i=list_i+1
    }
  }
}

# y_pos=time*init_y-time*(time-1)/2






