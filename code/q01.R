library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_1')

#1.1
test[,lag_v1:=shift(V1,type='lag')]
test[,.N,by=.(is.finite(lag_v1)&V1>lag_v1)]

#1.2
test[,lag3_v1:=shift(V1,type='lag',3)]
test[,.N,by=.(is.finite(lag3_v1)&V1>lag3_v1)]