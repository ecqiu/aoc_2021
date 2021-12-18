library(data.table)

stars=fread('/home/eqiu/code_projects/aoc_2021/data/input_13',header=F)
instros=fread('/home/eqiu/code_projects/aoc_2021/data/input_13',header=F,colClasses='character',skip=952)
# stars=data.table(
#   V1=c(
#     6
#     ,0
#     ,9
#     ,0
#     ,10
#     ,4
#     ,6
#     ,6
#     ,4
#     ,0
#     ,10
#     ,3
#     ,3
#     ,8
#     ,1
#     ,2
#     ,8
#     ,9
#   ),
#   V2=c(
#     10
#     ,14
#     ,10
#     ,3
#     ,4
#     ,11
#     ,0
#     ,12
#     ,1
#     ,13
#     ,12
#     ,4
#     ,0
#     ,4
#     ,10
#     ,14
#     ,10
#     ,0
#   )
# )

stars[,`:=`(V1=V1+1,V2=V2+1)]

star_mat=matrix(ncol=max(stars$V2),nrow=max(stars$V1))
star_mat[]=0
# dim(as.matrix(stars))


star_mat[as.matrix(stars)]=1


instros[,type:=ifelse(grepl('x',V3),'x','y')]
instros[,fold_num:=as.numeric(gsub('(x|y)=(\\d+)','\\2',V3))+1]
# instros=data.table(type='y',fold_num=8)
#13.1
for(i in 1:nrow(instros)){
  print(i)
  instro=instros[i,]
  
  if(instro$type=='x'){
    nfold_length=nrow(star_mat)-(instro$fold_num)
    star_mat[(instro$fold_num-nfold_length):(instro$fold_num-1),]=star_mat[(instro$fold_num-nfold_length):(instro$fold_num-1),]+star_mat[nrow(star_mat):(instro$fold_num+1),]
    
    star_mat[(instro$fold_num+1):nrow(star_mat),]=-1
    star_mat=star_mat[1:(instro$fold_num-1),]
  }
  
  if(instro$type=='y'){
    nfold_length=ncol(star_mat)-(instro$fold_num)
    star_mat[,(instro$fold_num-nfold_length):(instro$fold_num-1)]=star_mat[,(instro$fold_num-nfold_length):(instro$fold_num-1)]+star_mat[,ncol(star_mat):(instro$fold_num+1)]
    star_mat[,(instro$fold_num+1):ncol(star_mat)]=-1
    star_mat=star_mat[,1:(instro$fold_num-1)]
    
  }
  
  if(i==1){
    print(sum(star_mat>0))
  }
}


star_mat_base=copy(star_mat)
star_mat[]='.'

star_mat[star_mat_base>0]='#'

t(star_mat)



