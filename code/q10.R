library(data.table)

test=fread('/home/eqiu/code_projects/aoc_2021/data/input_10',header=F)


str=test$V1[1]


brack_pair=function(c){
  if(c=='('){return(')')}
  if(c=='['){return(']')}
  if(c=='{'){return('}')}
  if(c=='<'){return('>')}
  if(c==')'){return('(')}
  if(c==']'){return('[')}
  if(c=='}'){return('{')}
  if(c=='>'){return('<')}
  
}


check_str=function(str){

open_char=''
for(i in 1:nchar(str)){
  # print(open_char)
  c=substr(str,i,i)
  # print(c)
  
  if(c %in% c('(','[','{','<')){
    open_char=c(c,open_char)
  }
  
  
  if(c %in% c(')',']','}','>')){
    if(brack_pair(c)==open_char[1]){
      open_char=open_char[-1]
    }else{
      return(c)
    }
    
  }
}

return('x')

}

#10.1
out=data.table(err_char=unlist(lapply(test$V1,check_str)))

out[,score:=ifelse(err_char==')',3,ifelse(err_char==']',57,ifelse(err_char=='}',1197,ifelse(err_char=='>',25137,0))))]
out[,sum(score)]

#10.2
out[,index:=1:.N]
incomp_strs=test[out[score==0,]$index]

check_str2=function(str){
  
  open_char=''
  for(i in 1:nchar(str)){
    # print(open_char)
    c=substr(str,i,i)
    # print(c)
    
    if(c %in% c('(','[','{','<')){
      open_char=c(c,open_char)
    }
    
    
    if(c %in% c(')',']','}','>')){
      if(brack_pair(c)==open_char[1]){
        open_char=open_char[-1]
      }else{
        return(c)
      }
      
    }
  }
  
  return(open_char)
  
}
check_str2(incomp_strs[3])

complete_strs=lapply(incomp_strs$V1,check_str2)

score_str=function(str){
  str=str[1:(length(str)-1)]
  
  str=gsub('\\(','1',str)
  str=gsub('\\[','2',str)
  str=gsub('\\{','3',str)
  str=gsub('<','4',str)
  str=as.numeric(str)
  
  replace=5^(length(str):1-1)
  
  return(sum(replace*str))
}
median(unlist(lapply(complete_strs,score_str)))
