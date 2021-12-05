library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_4',colClasses='character',fill=T)

number_seq=unlist(test[1,])

test2=fread('/home/eqiu/code_projects/aoc_2021/data/input_4',colClasses='character',fill=T,skip=1)
bingo_boards=test2[1:nrow(test2)][V1!='',.(V1,V2,V3,V4,V5)]
bingo_boards[,board_num:=floor((1:.N-1)/5)]

boards_checked=matrix(rep(0,2500),nrow=500,ncol=5)

bingo_scores=data.table(board_num=0:99,score=0,finish_pos=0,finished=F,finish_i=0)
j=1
for(i in 1:100){
  print(i)
  boards_checked[as.matrix(bingo_boards[,.(V1,V2,V3,V4,V5)])==number_seq[i]]=1
  
  boards_checked_dt=data.table(boards_checked)
  boards_checked_dt[,board_num:=floor((1:.N-1)/5)]
  
  row_sums=boards_checked_dt[,rowSums(.SD),by=board_num]
  col_sums=boards_checked_dt[,as.list(colSums(.SD)),by=board_num]
  # diag_sums=boards_checked_dt[,sum(diag(as.matrix(.SD))),by=board_num]
  # diag_sums2=boards_checked_dt[,sum(diag(as.matrix(.SD)[,c(5:1)])),by=board_num]
  finished_boards=bingo_scores[finished==T]$board_num
  
  row_bingos=unique(floor((which(row_sums$V1==5)-1)/5))
  col_bingos=which(col_sums[,pmax(V1,V2,V3,V4,V5)]==5)-1
  # diag_bingos=which(diag_sums$V1==5)-1
  # diag_bingos2=which(diag_sums2$V1==5)-1
  
  new_bingos=setdiff(unique(c(row_bingos,col_bingos)),finished_boards )
  if(length(new_bingos)>0){
    for(board_numx in new_bingos){
      bingo_scores[board_num==board_numx,`:=`(
        finish_pos=j
        ,finished=T
        ,score=sum(as.numeric(as.matrix(bingo_boards[board_num==board_numx,.(V1,V2,V3,V4,V5)])[as.matrix(boards_checked_dt[board_num==board_numx,.(V1,V2,V3,V4,V5)]=='0')]))*as.numeric(number_seq[i])
        ,finish_i=i
      )]
      
      print(as.matrix(bingo_boards[board_num==board_numx,.(V1,V2,V3,V4,V5)])[as.matrix(boards_checked_dt[board_num==board_numx,.(V1,V2,V3,V4,V5)]=='0')])
      j=j+1
    }
  }
  print(boards_checked_dt[board_num==34])
}



