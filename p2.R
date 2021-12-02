library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_2')


#2.1
test[,depth:=ifelse(V1=='down',V2,-V2)]
test[V1=='forward',depth:=0]
test[,pos:=ifelse(V1=='forward',V2,0)]

prod(test[,.(sum(depth),sum(pos))])

#2.2
test[,aim:=cumsum(depth)]
test[,depth2:=aim*pos]

prod(test[,.(sum(depth2),sum(pos))])
