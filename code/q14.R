library(data.table)

polymer=fread('/home/eqiu/code_projects/aoc_2021/data/input_14',nrows=1,header=F)
rules=fread('/home/eqiu/code_projects/aoc_2021/data/input_14',header=F)

# rules$V1



poly=polymer$V1

#14.1
update_poly=function(in_poly){
  out_poly=c(substr(in_poly,1,1))
  for(i in 1:(nchar(in_poly)-1)){
    pair=substr(in_poly,i,(i+1))
    if(pair %in% rules$V1){
      out_poly=c(out_poly,rules[V1==pair]$V3,substr(in_poly,i+1,i+1))
    }else{
      out_poly=c(out_poly,substr(in_poly,i+1,i+1))
    }
    
  }
  
  return(paste0(out_poly,collapse=''))
}

poly2=poly
for(i in 1:10){
  print(i)
  # print(poly2)
  poly2=update_poly(poly2)
}

poly_string=tstrsplit(poly2,split='')

counts=data.table(unlist(poly_string))[,.N,by=V1]
max(counts$N)-min(counts$N)

#14.2
rules[,`:=`(
  out1=paste0(substr(V1,1,1),V3),
  out2=paste0(V3,substr(V1,2,2))
)]

poly=polymer$V1
start_table=data.table(unlist(tstrsplit(polymer$V1,split='')))
start_table[,pair:=paste0(V1,shift(V1,type='lead'))]
start_pairs=start_table[1:(nrow(start_table)-1),.N,by=pair]


#do updates at pair level instead
update_poly_pairs=function(pair_table){
  out_table=data.table(
    pair=unique(c(rules$out1,rules$out2,pair_table$pair)),
    N=0
  )
  
  for(i in 1:nrow(pair_table)){
    entry_pair=pair_table[i,]$pair
    if(entry_pair%in% rules$V1){
      out_table[pair==rules[V1==entry_pair]$out1, N:=N+pair_table[i,]$N]
      out_table[pair==rules[V1==entry_pair]$out2, N:=N+pair_table[i,]$N]
    }else{
      out_table[pair==entry_pair, N:=N+pair_table[i,]$N]
    }
  }
  
  return(out_table)

}
#loop 40 times
start_pair2=copy(start_pairs)
for(i in 1:40){
  print(i)
  start_pair2=update_poly_pairs(start_pair2)
}


#was double counting all the letters except start+end letters, so add 1 to double count start/end letters
start_pair2[,`:=`(l1=substr(pair,1,1),l2=substr(pair,2,2))]


counts2=rbindlist(list(
  start_pair2[,.(N=sum(N)),by=.(l=l1)]
  ,start_pair2[,.(N=sum(N)),by=.(l=l2)]
))[,.(N=sum(N)),by=l]
counts2[l==substr(poly,1,1),N:=N+1]
counts2[l==substr(poly,nchar(poly),nchar(poly)),N:=N+1]

as.character((max(counts2$N)-min(counts2$N))/2)


