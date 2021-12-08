library(data.table)

test=unlist(fread('/home/eqiu/code_projects/aoc_2021/data/input_7',fill=T))

#7.1
sum(abs(test-median(test)))

#7.2
gauss=function(x){
  return(x*(x+1)/2)
}
sum(gauss(abs(test-round(mean(test))-1)))
#bit of a guess but the n^2 term should dominate, so check round(mean(test)) and numbers around it

