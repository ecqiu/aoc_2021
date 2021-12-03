library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_3',colClasses='character')


#3.1
decimals=as.data.table(tstrsplit(test$V1,split=''))
num=paste0(unlist(lapply(decimals,function(x) median(as.numeric(x)))),collapse='')
num2=paste0(unlist(lapply(decimals,function(x) ifelse(median(as.numeric(x))==1,0,1))),collapse='')

strtoi(num,base=2)*strtoi(num2,base=2)

#3.2
decimals[,string:=paste0(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)]

pos=1
tb=decimals
while(nrow(tb)>1){
  keep_val=median(as.numeric(tb[[paste0('V',pos)]]))
  if( keep_val==0.5){keep_val=1}
  tb=tb[get(paste0('V',pos))==keep_val]
  pos=pos+1
}

pos=1
tb2=decimals
while(nrow(tb2)>1){
  keep_val=ifelse(median(as.numeric(tb2[[paste0('V',pos)]]))==0,1,0)
  if( keep_val==0.5){keep_val=0}
  tb2=tb2[get(paste0('V',pos))==keep_val]
  pos=pos+1
}

strtoi(tb$string,base=2)*strtoi( tb2$string,base=2)
