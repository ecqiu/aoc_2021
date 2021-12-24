library(data.table)
monad=fread('/home/eqiu/code_projects/aoc_2021/data/input_24',header=F,fill=T)

##
#need to read input and understand it's in 14 blocks of 1+17lines (1input).
#the 17 lines don't differ, except the numbers in input 4,5 and 15
#update_reg2 in comments below shows how the function works
#can manually invert the function to find solutions for input z, for a given input w+output z (x,y inputs are ignored by the function)
#for loop manually inverts below, and dedup to find largest(part1)/smallest(part2) output z for any given input z
##
lookup=data.table(ind=1:14)
for(i in 1:14){
  inp_ind=(i-1)*18+1
  rows=monad[(inp_ind+1):(inp_ind+17),]
  
  lookup[i,`:=`(val1=as.numeric(rows[4,]$V3),val2=as.numeric(rows[5,]$V3),val3=as.numeric(rows[15,]$V3))]
}
setkey(lookup,ind)



##############################
digit_list=list()
digit_list[[15]]=list(0,'')
for(i in 14:1){
  print(i)
  val1=lookup[i,val1]
  val2=lookup[i,val2]
  val3=lookup[i,val3]
  out_digits=c()
  digit_list[[i]]=list(numeric(0),character(0))
  if(length(digit_list[[i+1]][[1]])==0){
    break
  }
  for(j in 1:length(digit_list[[i+1]][[1]])){
    z_out=digit_list[[i+1]][[1]][j]
    for(w in 1:9){
      if(val1==26){
        range=c((z_out-(w-val2%%26)%/%26 )*26 + (w-val2%%26) )
        if((z_out-w-val3)%%26==0){
          ind=(z_out-w-val3)/26
          add_range=(ind*26):((ind+1)*26-1)
          add_range=add_range[!(add_range%%26+val2==w)]
          range=c(range,add_range)
        }
      }else{
        range=c()
        if((z%%26+val2)==w){
          range=c(range,z_out)
        }
        if((z%%26+val2)!=w && (z_out-w-val3)%%26==0){
          range=c(range,(z_out-w-val3)/26)
        }
      }
      
      if(length(range>0)){
        # digit_list[[i]][[1]]=c(digit_list[[i]][[1]],rep(w,length(range)))
        digit_list[[i]][[1]]=c(digit_list[[i]][[1]],range)
        digit_list[[i]][[2]]=c(digit_list[[i]][[2]],paste0( rep(w,length(range)) ,digit_list[[i+1]][[2]][j]))
      }
    }
  }
  temp_dt=as.data.table(digit_list[[i]])
  temp_dt=temp_dt[order(V1,-V2)] #24.1
  # temp_dt=temp_dt[order(V1,V2)] #24.2
  
  temp_dt=unique(temp_dt,by='V1')
  digit_list[[i]][[1]]=temp_dt$V1
  digit_list[[i]][[2]]=temp_dt$V2
  
  print(length(digit_list[[i]][[1]]))
}


# update_reg2=function(reg,i){
#   w=reg$w
#   x=reg$x
#   y=reg$y
#   z=reg$z
#   
#   x=z%%26+as.numeric(lookup[i,]$val2)
#   x=as.numeric(x!=w)
#   
#   y=25*x+1
#   z=(z%/%as.numeric(lookup[i,]$val1))*y
#   
#   y=(w+as.numeric(lookup[i,]$val3))*x
#   z=z+y
#   
#   return(list(w=w,x=x,y=y,z=z))
# }
# 
# #####################################
# z_fin=(z%/%val1)*(25*((z%%26+val2)!=w)+1)+(w+val3)*((z%%26+val2)!=w)
# 
# #val1=1
# ##(z%%26+val2)!=w
# z_fin=z*26+w+val3
# -->z=(z_fin-w-val3)/26
# 
# 
# ##(z%%26+val2)==w
# z_fin=z
# ############################################
# #val1=26
# ##(z%%26+val2)!=w
# z_fin=(z%/%26)*26+w+val3
# 
# z_fin-w-val3=(z%/%26)*26
# 
# 
# 
# 
# ##(z%%26+val2)==w
# z_fin=z%/%26
# 
# z%%26=w-val2
# -->z= (z_fin-(w-val2%%26)%/%26 )*26 + (w-val2%%26) 
# 
# # z_fin==0
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####################################
# 
# z_fin=z*26+(w+val3)
# 
# (z_fin-w-val3)/26
# 
# 
# (digit_list[[11]][[2]]-digit_list[[11]][[1]]-lookup[10,]$val3)/26
# 
# ####################
# z%%26+val2!=w
# -->
#   (z%/%val1)*(25+1)=-(w+val3)
# #never happens
# #####################
# z%%26+val2==w
# 
# z%/%val1==0
# ###################################
# z*26=-(w+val3)
# 
# 
# (z%/%val1)*(25*((z%%26+val2)!=w)+1)=-(w+val3)*((z%%26+val2)!=w)
# 
# 
# z_fin=(z%/%val1)*(25*((z%%26+val2)!=w)+1)+(w+val3)*((z%%26+val2)!=w)
# 
# #
# z_fin=z*(25*((z%%26+val2)!=w)+1)+(w+val3)*((z%%26+val2)!=w)
# 
# 
# ###
# z_fin=z*26+(w+val3)
# z_fin=z
# 
# 
# 
# ##
# (z%%26+val2)!=w
# 
# z_fin=(z%/%val1)*26+(w+val3)#<--
# 
# (z%/%val1)*26=z_fin-(w+val3)
# 
# # w=z_fin-val3 - (z%/%val1)*26
# # 
# # z_fin>(val3 - z%/%val1*26)
# # 
# # if(val1==1){
# #   z=0
# #   w=z_fin-val3
# # }
# # 
# # 
# # if(val1==26){
# #   z=0
# #   w=z_fin-val3
# # }
# # 
# # (z%/%val1)*26 <z_fin-val3
# 
# ###
# (z%%26+val2)==w
# 
# (z%/%val1)*1=z_fin
# 
# 
# 
# 
# 
# 
# z_fin-val3
# 
# z_out-
# z==0

# 
# invert_block(val1,val2,val3,z){
#   if(val1==26){
#     w=1:9
#     z=w-val2
#     z=c(z-52,z-26,z,z+26,z+52)
#     z=z[z<26&z>=0]
#   }
#   
#   if(val1==1){
#     # w=-val3
#     z=w-val2
#     mod=26
#   }
#   
#   return(list(z=z,w=w))
# }
  
# for(i in 1:14){
#   inp_ind=(i-1)*18+1
#   rows=monad[(inp_ind+1):(inp_ind+17),]
#   
#   print(which(!rows$V3==monad[(1+1):(1+17),]$V3))
#   # print(rows)
# }




# mem <<- list()
# update_reg=function(reg,i){
#   key=paste0(c(
#     paste0(unlist(reg),collapse='_'),
#     i
#   ),collapse='|')
#   
#   inp_ind=(i-1)*18+1
#   rows=monad[(inp_ind+1):(inp_ind+17),]
#   if(key %in% names(mem) &row$V1!='inp'){
#     return(mem[[key]])
#   }
#   
#   for(j in 1:nrow(rows)){
#     row=rows[j,]
#     if(row$V3 %in% c('w','x','y','z')){
#       b_input=reg[[row$V3]]  
#     }
#     else{
#       b_input=as.numeric(row$V3)
#     }
#       
#     # print(row)
#     # print(b_input)
#     # print(reg[[row$V2]])
#     # print(reg)
#     
#     if(row$V1=='add'){
#       reg[[row$V2]]=reg[[row$V2]]+b_input
#     }
#     if(row$V1=='mul'){
#       reg[[row$V2]]=reg[[row$V2]]*b_input
#     }
#     if(row$V1=='div'){
#       reg[[row$V2]]=reg[[row$V2]]%/%b_input
#     }
#     if(row$V1=='mod'){
#       reg[[row$V2]]=reg[[row$V2]]%%b_input
#     }
#     if(row$V1=='eql'){
#       reg[[row$V2]]=as.numeric(reg[[row$V2]]==b_input)
#     }
#   }
#   
#   if(row$V1!='inp'){
#     mem[[key]]<<-reg
#   }
#   return(reg)
# }
# update_reg2=function(reg,i){
#   if(i==1){
#     return(list(w=reg$w,x=1,y=16+reg$w,z=16+reg$w)
#   }
#   if(i==2){
#     return(list(w=reg$w,x=1,y=11+reg$w,z=26*reg$z+11+reg$w))
#   }
#   if(i==2){
#     return(list(w=reg$w,x=x,y=12+reg$w,z=26*reg$z+12+reg$w))
#   }
#   
# }

sub_number='13579246899999'
mem2 <<- list()
monad_check=function(sub_number){
  reg=list(
    w=0
    ,x=0
    ,y=0
    ,z=0
  )
  sub_num_ind=1
  key_list=c()
  for(i in 1:14){
    # print(i)
    # key=paste0(c(
    #   paste0(unlist(reg),collapse='_'),
    #   i
    # ),collapse='|')
    # 
    # if(key %in% names(mem2)){
    #   return(mem2[[key]])  
    # }
    # 
    # key_list=c(key)
    # print(i)
    inp_ind=(i-1)*18+1
    # inp=monad[(i-1)*18+1,]
    reg[['w']]=as.numeric(substr(sub_number,i,i))
    
    reg=update_reg2(reg,i)
    
    
    # if(row$V3 %in% c('w','x','y','z')){
    #   b_input=reg[[row$V3]]  
    # }
    # else{
    #   b_input=as.numeric(row$V3)
    # }
    # 
    # 
    # if(row$V1=='inp'){
    #   reg[[row$V2]]=as.numeric(substr(sub_number,sub_num_ind,sub_num_ind))
    #   sub_num_ind=sub_num_ind+1  
    # }
    # if(row$V1=='add'){
    #   reg[[row$V2]]=reg[[row$V2]]+b_input
    # }
    # if(row$V1=='mul'){
    #   reg[[row$V2]]=reg[[row$V2]]*b_input
    # }
    # if(row$V1=='div'){
    #   reg[[row$V2]]=reg[[row$V2]]%/%b_input
    # }
    # if(row$V1=='mod'){
    #   reg[[row$V2]]=reg[[row$V2]]%%b_input
    # }
    # if(row$V1=='eql'){
    #   reg[[row$V2]]=as.numeric(reg[[row$V2]]==b_input)
    # }
  }
  # print(reg)
  mem2[key_list]<<- reg$z
  # print(sub_num_ind)
  # print(reg$z)
  # print(reg)
  return(reg$z)
}

# init=99999999999999+1
# 
# out_check=1
# num=init
# 
# nums=c()
# checks=c()
# while(out_check!=0){
#   num=num-1
#   # print(as.character(num))
#   
#   
#   char_num=as.character(num)
#   if(grepl('0',char_num)){
#     # num=num-1
#     next
#   }
#   out_check=monad_check(char_num)  
#   nums=c(nums,char_num)
#   checks=c(checks,out_check)
#   if((num %% 1000)==0){
#     print(num)
#     print(out_check)
#   }
# }
# 
# check_dt=data.table(num=nums,check=checks)
# check_dt[order(check)][1:50]
# 
# 
# # monad_check('11111111111111')
# monad_check(as.character(as.numeric(paste0(rep(1,14),collapse=''))+6))




