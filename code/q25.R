library(data.table)
cucumbers=fread('/home/eqiu/code_projects/aoc_2021/data/input_25',header=F,fill=T)


# cucumbers=list(V1=c(
#   'v...>>.vv>',
#   '.vv>>.vv..',
#   '>>.>v>...v',
#   '>>v>>.>.v.',
#   'v>v.vv.v..',
#   '>.>>..v...',
#   '.vv..>.>v.',
#   'v.v..>>v.v',
#   '....v..v.>'
# ))

cucumber_mat=t(as.matrix(as.data.table(strsplit(cucumbers$V1,split=''))))

souths_init=which(cucumber_mat=='v')
easts_init=which(cucumber_mat=='>')

nrow(cucumber_mat)
ncol(cucumber_mat)

steps=1
changes=c(-1)

souths=souths_init
easts=easts_init

while(length(changes)>0){
  print(steps)
  changes=c()
  easts_change=ifelse( ((easts-1)%/%nrow(cucumber_mat)) ==(ncol(cucumber_mat)-1), (easts-1)%%nrow(cucumber_mat)+1,easts+nrow(cucumber_mat)  )
  easts_next=ifelse(easts_change %in% c(souths,easts),easts,easts_change)
  changes=c(changes,setdiff(easts_next,easts))
  
  easts=easts_next
  
  souths_change=ifelse( ((souths)%%nrow(cucumber_mat)) == 0 , ((souths-1)%/%nrow(cucumber_mat))*nrow(cucumber_mat)+1 ,souths+1  )
  souths_next=ifelse(souths_change %in% c(souths,easts),souths,souths_change)
  changes=c(changes,setdiff(souths_next,souths))
  
  souths=souths_next
  print(length(changes))  
  steps=steps+1
}
