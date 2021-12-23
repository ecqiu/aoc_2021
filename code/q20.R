library(data.table)
pixs=fread('/home/eqiu/code_projects/aoc_2021/data/input_20',header=F)
map=pixs$V1[1]
pic_mat=t(as.matrix(as.data.table(strsplit(pixs$V1[3:102],split=''))))

# map=gsub('\\s','','..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
# #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
# .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
# .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
# .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
# ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
# ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#')
# 
# pict_mat_in=c(
#   '#..#.',
#   '#....',
#   '##..#',
#   '..#..',
#   '..###'
# )
# pic_mat=t(as.matrix(as.data.table(strsplit(pict_mat_in,split=''))))


####
coords=which(pic_mat=='#')
# coords_vec=lapply(coords,function(x) c((x-1)%%nrow(pic_mat)+1,(x-1)%/%nrow(pic_mat)+1))
coords_dt=data.table(
  x=unlist(lapply(coords,function(x) (x-1)%/%nrow(pic_mat)+1 )),
  y=-unlist(lapply(coords,function(x) (x-1)%%nrow(pic_mat)+1))
)
####


#21.1
# adjs=function(coord){
#   outs=data.table(sweep(expand.grid(-1:1,1:-1),2,-coord))
#   names(outs)=c('x','y')
#   return(outs)
# }



map_pixel=function(coord,coord_list,coord_light=T){
  # outs=data.table(sweep(expand.grid(-1:1,1:-1),2,-coord))
  # outs=rbindlist(lapply(1:nrow(coord),function(x) sweep(expand.grid(-1:1,1:-1),2,-unlist(coord[x,] ))))
  outs=coord[,.(x=expand.grid(-1:1,1:-1)[[1]]+.SD$x,y=expand.grid(-1:1,1:-1)[[2]]+.SD$y)  ,by=1:nrow(coord)][,.(x,y)]
  
  names(outs)=c('x','y')
  outs[,index:=ceiling((1:.N)/9)]
  
  
  
  
  # outs[,coord_key:=paste0(x,'_',y)]
  if(coord_light==T){
    outs[,string:=0]
    outs[coord_list,string:=1,on=c('x','y')]
  }else{
    outs[,string:=1]
    outs[coord_list,string:=0,on=c('x','y')]
    # outs[,string:=ifelse(coord_key%in% coord_list,0,1)]
  }
  string_tables=outs[,paste0(.SD$string,collapse=''),by=index]
  string_tables[,lookup:=strtoi(V1,2L)+1]
  
  # lookup=strtoi(paste0(outs$string,collapse=''),base=2L)
  return(unlist(lapply(string_tables$lookup,function(x) substr(map,x,x))))
}
# map_pixel(adj_coords[1,])

# coord=unlist(adj_coords[1,])
# coord_list=paste0(pixels$x,'_',pixels$y)


update_pixels=function(pixels,coord_light=T){
  adj_coords=pixels[,.(x=expand.grid(-1:1,1:-1)[[1]]+.SD$x,y=expand.grid(-1:1,1:-1)[[2]]+.SD$y)  ,by=1:nrow(pixels)][,.(x,y)]
  adj_coords=unique(adj_coords)
  # adj_coords[,next_pix:=unlist(lapply(1:nrow(adj_coords),function(i) map_pixel(unlist(adj_coords[i,]),coord_list=pixels,coord_light=coord_light )))]
  adj_coords[,next_pix:=map_pixel(adj_coords,coord_list=pixels,coord_light=coord_light )]
  
  
  if(coord_light){
    return(adj_coords[next_pix=='.',.(x,y)])
  }else{
    return(adj_coords[next_pix=='#',.(x,y)])
  }
  # out=adj_coords[next_pix=='#',.(x,y)]
  # out=rbindlist(list(out,pixels))
  # out=unique(out)
  return(out)
}
#24

# temp1=update_pixels(coords_dt,T)
# temp2=update_pixels(temp1,T)
temp2=copy(coords_dt)
temp1=update_pixels(temp2,T)
temp2=update_pixels(temp1,F)

#####################################3
#21.2
temp2=copy(coords_dt)
for(i in 1:25){
  print(i)
  # setkeyv(temp2,c('x','y'))
  temp1=update_pixels(temp2,T)
  # setkeyv(temp1,c('x','y'))
  temp2=update_pixels(temp1,F)
  print(nrow(temp2))
}
print(nrow(temp2))