library(data.table)
instrs=fread('/home/eqiu/code_projects/aoc_2021/data/input_22',header=F)


instrs[,`:=`(
  x1=as.numeric(gsub('.*x=(.*)\\.\\.(.*)','\\1',V1)),
  x2=as.numeric(gsub('.*x=(.*)\\.\\.(.*)','\\2',V1)),
  y1=as.numeric(gsub('.*y=(.*)\\.\\.(.*)','\\1',V2)),
  y2=as.numeric(gsub('.*y=(.*)\\.\\.(.*)','\\2',V2)),
  z1=as.numeric(gsub('.*z=(.*)\\.\\.(.*)','\\1',V3)),
  z2=as.numeric(gsub('.*z=(.*)\\.\\.(.*)','\\2',V3)),
  instr=gsub('^(\\w+).*','\\1',V1)
)]
cubes_small=array(0,c(101,101,101))
# instrs=data.table(
#   x1=c(-5,-44,-49,-20,26,-41,-43,-33,35,-14),x2=c(47,5,-1,34,39,5,-33,15,47,36),
#   y1=c(-31,-27,-11,-40,40,-41,-45,-32,-46,-6),y2=c(22,21,42,6,50,6,-28,19,-34,44),
#   z1=c(-19,-14,-10,-44,-2,-36,7,-34,-11,-16),z2=c(33,35,38,1,11,8,25,11,5,29),
#   instr=c('on','on','on','on','off','on','off','on','off','on')
# )
# # cubes_small[1,1,1:5]
##22.1
for(i in 1:nrow(instrs)){
  print(i)
  row=instrs[i,]
  
  x1=row$x1
  x2=row$x2
  y1=row$y1
  y2=row$y2
  z1=row$z1
  z2=row$z2
  
  if(x2< -50 | x1>50 | y2< -50 | y1>50 |z2< -50 | z1>50){
    # print(row)
    next
  }
  
  if(x2>50&x1<50){
    print(row)
  }
  
  x1=pmax(pmin(x1,50),-50)
  x2=pmax(pmin(x2,50),-50)
  y1=pmax(pmin(y1,50),-50)
  y2=pmax(pmin(y2,50),-50)
  z1=pmax(pmin(z1,50),-50)
  z2=pmax(pmin(z2,50),-50)
  
  
  if(row$instr=='on'){
    cubes_small[x1:x2+51,y1:y2+51,z1:z2+51]=1
  }
  
  if(row$instr=='off'){
    cubes_small[x1:x2+51,y1:y2+51,z1:z2+51]=0
  }
  print(sum(cubes_small))
}




##22.2
# instrs2=instrs[!(x2< -50 | x1>50 | y2< -50 | y1>50 |z2< -50 | z1>50)]
# instrs=instrs[!(x2< -50 | x1>50 | y2< -50 | y1>50 |z2< -50 | z1>50)]
# instrs=data.table(x1=c(10,11,8,10),x2=c(12,13,11,10),y1=c(10,11,8,10),y2=c(12,13,11,10),z1=c(10,11,8,10),z2=c(12,13,11,10),instr=c('on','on','off','on'))



cube_list_plus=data.table()
cube_list_minus=data.table()
for(i in 1:nrow(instrs)){
  print(i)
  row=instrs[i,]
  if(row$instr=='on'){#if input =on, subtract plus, add minus then add new cube ([1]-1+1=1, [0]-0+1=1)
    # ons=ons+(row$x2-row$x1+1)*(row$y2-row$y1+1)*(row$z2-row$z1+1)
    cube_list_minus_new=data.table()
    cube_list_plus_new=data.table()
    if(nrow(cube_list_plus)>0){
    for(j in 1:nrow(cube_list_plus)){
      cube2=cube_list_plus[j,]
      
      x1=max(cube2$x1,row$x1)
      x2=min(cube2$x2,row$x2)
      y1=max(cube2$y1,row$y1)
      y2=min(cube2$y2,row$y2)
      z1=max(cube2$z1,row$z1)
      z2=min(cube2$z2,row$z2)
      
      if((x2-x1)>0 & (y2-y1)>0&(z2-z1)>0){
        cube_list_minus_new=rbindlist(list(cube_list_minus_new,data.table(x1=x1,x2=x2,y1=y1,y2=y2,z1=z1,z2=z2,n=cube2$n)))
      }
    }
    }
    
    if(nrow(cube_list_minus)>0){
    for(j in 1:nrow(cube_list_minus)){#if input =on, subtract plus, add minus only ([1]-1=0, [0]-0=0)
      cube2=cube_list_minus[j,]

      x1=max(cube2$x1,row$x1)
      x2=min(cube2$x2,row$x2)
      y1=max(cube2$y1,row$y1)
      y2=min(cube2$y2,row$y2)
      z1=max(cube2$z1,row$z1)
      z2=min(cube2$z2,row$z2)

      # ons=ons-as.numeric(length(x_ints))*as.numeric(length(y_ints))*as.numeric(length(z_ints))
      if((x2-x1)>0 & (y2-y1)>0&(z2-z1)>0){
        cube_list_plus_new=rbindlist(list(cube_list_plus_new,data.table(x1=x1,x2=x2,y1=y1,y2=y2,z1=z1,z2=z2,n=cube2$n)))
      }
    }
    }
    
    cube_list_plus=rbindlist(list(cube_list_plus,cube_list_plus_new))
    cube_list_minus=rbindlist(list(cube_list_minus,cube_list_minus_new))
    cube_list_plus=rbindlist(list(cube_list_plus,row[,.(x1,x2,y1,y2,z1,z2,n=1)]))
    # print(cube_list_plus)
    
    
    
  }
  
  if(row$instr=='off'){
    # ons=ons+(row$x2-row$x1+1)*(row$y2-row$y1+1)*(row$z2-row$z1+1)

    cube_list_minus_new=data.table()
    cube_list_plus_new=data.table()
    if(nrow(cube_list_plus)>0){
      for(j in 1:nrow(cube_list_plus)){
        cube2=cube_list_plus[j,]

        # x_ints=intersect(cube2$x1:cube2$x2,row$x1:row$x2)
        # y_ints=intersect(cube2$y1:cube2$y2,row$y1:row$y2)
        # z_ints=intersect(cube2$z1:cube2$z2,row$z1:row$z2)
        
        x1=max(cube2$x1,row$x1)
        x2=min(cube2$x2,row$x2)
        y1=max(cube2$y1,row$y1)
        y2=min(cube2$y2,row$y2)
        z1=max(cube2$z1,row$z1)
        z2=min(cube2$z2,row$z2)

        if((x2-x1)>0 & (y2-y1)>0&(z2-z1)>0){
          cube_list_minus_new=rbindlist(list(cube_list_minus_new,data.table(x1=x1,x2=x2,y1=y1,y2=y2,z1=z1,z2=z2,n=cube2$n)))
        }
      }
    }

    if(nrow(cube_list_minus)>0){
    for(j in 1:nrow(cube_list_minus)){
      cube2=cube_list_minus[j,]

      # x_ints=intersect(cube2$x1:cube2$x2,row$x1:row$x2)
      # y_ints=intersect(cube2$y1:cube2$y2,row$y1:row$y2)
      # z_ints=intersect(cube2$z1:cube2$z2,row$z1:row$z2)
      x1=max(cube2$x1,row$x1)
      x2=min(cube2$x2,row$x2)
      y1=max(cube2$y1,row$y1)
      y2=min(cube2$y2,row$y2)
      z1=max(cube2$z1,row$z1)
      z2=min(cube2$z2,row$z2)

      if((x2-x1)>0 & (y2-y1)>0&(z2-z1)>0){
        cube_list_plus_new=rbindlist(list(cube_list_plus_new,data.table(x1=x1,x2=x2,y1=y1,y2=y2,z1=z1,z2=z2,n=cube2$n)))
      }


    }
    }
    cube_list_plus=rbindlist(list(cube_list_plus,cube_list_plus_new))
    cube_list_minus=rbindlist(list(cube_list_minus,cube_list_minus_new))
  }
  
  
  
  if(nrow(cube_list_plus)>0){
    cube_list_plus=cube_list_plus[,.(n=sum(n)),by=.(x1,x2,y1,y2,z1,z2)]
    # cp_size=cube_list_plus[,sum((x2-x1+1)*(y2-y1+1)*(z2-z1+1)*n)]
  }else{
    # cp_size=0
  }
  
  if(nrow(cube_list_minus)>0){
    cube_list_minus=cube_list_minus[,.(n=sum(n)),by=.(x1,x2,y1,y2,z1,z2)]
    # cm_size=cube_list_minus[,sum((x2-x1+1)*(y2-y1+1)*(z2-z1+1)*n)]
  }else{
    # cm_size=0
  }
  
  # if(nrow(cube_list_minus)>0&nrow(cube_list_plus)>0){
  #   cube_list_plus[,key:=paste0(x1,'_',x2,'_',y1,'_',y2,'_',z1,'_',z2)]
  #   cube_list_minus[,key:=paste0(x1,'_',x2,'_',y1,'_',y2,'_',z1,'_',z2)]
  #   
  #   inter_keys=intersect(cube_list_plus$key,cube_list_minus$key)
  #   cube_list_plus[]
  #   
  # }
    
  
  # print(cp_size-cm_size)
  # print(nrow(cube_list_plus))
  # print(nrow(cube_list_minus))
}
cp_size=cube_list_plus[,sum((x2-x1+1)*(y2-y1+1)*(z2-z1+1)*n)]
cm_size=cube_list_minus[,sum((x2-x1+1)*(y2-y1+1)*(z2-z1+1)*n)]
print(as.character(cp_size-cm_size))


