library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_5',colClasses='character',fill=T)


test[,`:=`(
  x1=as.numeric(V1),
  y1=as.numeric(gsub('(\\d+) -> (\\d+)','\\1',V2)),
  x2=as.numeric(gsub('(\\d+) -> (\\d+)','\\2',V2)),
  y2=as.numeric(V3)
)]


#5.1
line_grid=matrix(rep(0,10^6),nrow=1000,ncol=1000)

lines=test[x1==x2|y1==y2]
for(i in 1:nrow(lines)){
  l=lines[i,]
  line_grid[l$x1:l$x2,l$y1:l$y2]=line_grid[l$x1:l$x2,l$y1:l$y2]+1
}

sum(line_grid>=2)

#5.2
line_grid=matrix(rep(0,10^6),nrow=1000,ncol=1000)
lines=test
for(i in 1:nrow(lines)){
  l=lines[i,]
  if(l$x1==l$x2|l$y1==l$y2){
    line_grid[l$x1:l$x2,l$y1:l$y2]=line_grid[l$x1:l$x2,l$y1:l$y2]+1
  }else{
    
    xs=l$x1:l$x2
    ys=l$y1:l$y2
    
    coords=xs+(ys-1)*1000
    line_grid[coords]=line_grid[coords]+1
  }
}

sum(line_grid>=2)