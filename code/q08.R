library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_8',header=F)

#8.1
nchars=nchar(unlist(strsplit(test$V2,split=' ')))

sum(nchars %in%c(2,4,3,7))


# #8.2


decode=function(str){
  decode_map=as.list(c('a','b','c','d','e','f','g'))
  names(decode_map)=c('a','b','c','d','e','f','g')
  
  digits=unlist(strsplit(str,split=' '))
  
  char_list=data.table(char=unlist(strsplit(digits,split='')))[,.N,by=char]
  
  decode_map[['e']]=char_list[N==4]$char
  decode_map[['f']]=char_list[N==9]$char
  decode_map[['b']]=char_list[N==6]$char
  
  decode_map[['c']]=gsub(decode_map[['f']],'',digits[nchar(digits)==2])
  decode_map[['a']]=char_list[N==8&char!=decode_map[['c']]]$char
  
  decode_map[['d']]=gsub(paste0('(',paste0(c(decode_map[['b']],decode_map[['c']],decode_map[['f']]),collapse='|'),')'),'',  digits[nchar(digits)==4])
  
  decode_map[['g']]=char_list[N==7&char!=decode_map[['d']]]$char
  decode_map[[' ']]=' '
  
  l=as.list(names(decode_map))
  names(l)=unlist(decode_map)
  
  return(l)
}


digit_map=as.list(0:9)
names(digit_map)=c('abcefg','cf','acdeg','acdfg','bcdf','abdfg','abdefg','acf','abcdefg','abcdfg')
for(i in 1:nrow(test)){

  decode_map=decode(test$V1[i])
  str2=test$V2[i]
  
  decoded_strings=unlist(lapply(strsplit(paste(unlist(decode_map[strsplit(str2,split='')[[1]]]),collapse=''),split=' ')[[1]],
         function(x) paste0(sort(unlist(strsplit(x,''))),collapse='')
  ))
  
  
  
  test[i,val:=as.numeric(paste0(unlist(digit_map[decoded_strings]),collapse=''))]
}


# a_to_g=c('a','b','c','d','e','f','g')
# permutations=as.matrix(expand.grid(a_to_g,a_to_g,a_to_g,a_to_g,a_to_g,a_to_g,a_to_g))
# permutations=permutations[apply(permutations,1,function(x) length(unique(x))==7),]
# 
# alpha_string=unlist(strsplit('abcdefg',split=''))
# 
# alpha_perms=apply(permutations,2,function(x) alpha_string[x])
# 
# 
# test$V1
# 
# 
# permutations
# 
# # patterns=c('abcefg','cf','acdeg','acdfg','bcdf','abdfg','abdefg','acf','abcdefg','abcdfg')
# # perm=paste0(c('a','b','c','d','e','f','g'),collapse='')
# 
# map_string=function(str,perm){
#   perm_list=as.list(strsplit(perm,split='')[[1]])
#   names(perm_list)=c('a','b','c','d','e','f','g')
#   
# 
#   return(
#     paste0(sort( unlist(lapply(strsplit(str,split='')[[1]],function(x) perm_list[[x]]))),collapse='')
#   )
# }
# 
# map_string('abc','bacdefg')
# 
# str=test$V1[1]
# 
# #b,e,f
# #1 ->c+a
# #0-> abcef, g
# 
# c(8,6,8,7,4,9,7)