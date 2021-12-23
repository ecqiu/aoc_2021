library(data.table)
init_chambers=fread('/home/eqiu/code_projects/aoc_2021/data/input_23',header=F)


wait_slots=rep('',7)
room_slots=list('A'=c('B','B'),'B'=c('C','C'),'C'=c('A','D'),'D'=c('D','A'))
energy_vals=10^(0:3)
names(energy_vals)=c('A','B','C','D')



exec_move=function(move,wait_slots,room_slots){
  if(move[[1]][[1]]=='R'){
    x=room_slots[[ move[[1]][[2]] ]][move[[1]][[3]]  ]
    room_slots[[ move[[1]][[2]] ]][move[[1]][[3]]  ]=''
  }
  if(move[[1]][[1]]=='H'){
    x=wait_slots[move[[1]][[2]]  ]
    wait_slots[move[[1]][[2]]  ]=''
  }
  
  
  if(move[[2]][[1]]=='R'){
    room_slots[[ move[[2]][[2]] ]][move[[2]][[3]]  ]=x
  }
  if(move[[2]][[1]]=='H'){
    wait_slots[move[[2]][[2]]  ]=x
  }
  
  return(list(wait_slots=wait_slots,room_slots=room_slots))
    
}

wait_slots=c('','','','D','D','A','')
room_slots=list('A'=c('','A'),'B'=c('B','B'),'C'=c('C','C'),'D'=c('',''))

test=exec_move(move,wait_slots,room_slots)

hallway_pos=data.table(
  index=1:11,  
  name=c('1','2','A','3','B','4','C','5','D','6','7')
)
# hallway_pos

mem_inbetw_pos<<-list()
get_inbetween_pos=function(h_i,r){
  key=paste0(h_i,'_',r)
  if(key %in% names(mem_inbetw_pos)){
    return(mem_inbetw_pos[[key]])
  }
  
  min_ind=min(hallway_pos[name==h_i]$index,hallway_pos[name==r]$index)+1
  max_ind=max(hallway_pos[name==h_i]$index,hallway_pos[name==r]$index)-1
  
  if(min_ind<=max_ind){
    inbetween_hallway_pos=(min_ind):(max_ind)
    inbetween_hallway_pos=hallway_pos[index %in%inbetween_hallway_pos]$name
    inbetween_hallway_pos=as.numeric(grep('\\d+',inbetween_hallway_pos,value=T))
  }else{
    inbetween_hallway_pos=numeric(0)
  }
  
  mem_inbetw_pos[[key]]<<-inbetween_hallway_pos
  
  return(inbetween_hallway_pos)
}

list_legal_moves=function(wait_slots,room_slots){
  out_list=list()
  out_energy=c()
  for(i in 1:7){
    move=list()
    if(wait_slots[i]!=''){
      x=wait_slots[i]
      
      move[[1]]=list()
      move[[1]][[1]]='H'
      move[[1]][[2]]=i
      
      
      inbetween_hallway_pos=get_inbetween_pos(i,x)
      
      if(all(room_slots[[x]]%in%c('',x)) && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]=='')) ){
        move[[2]]=list()
        move[[2]][[1]]='R'
        move[[2]][[2]]=x
        move[[2]][[3]]=max(which(room_slots[[x]]==''))
        
        out_list=c(out_list,list(move))
        eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==x]$index) +move[[2]][[3]])
          
        out_energy=c(out_energy,eval_out)
      }
      
      # if(room_slots[[x]][2]==x && room_slots[[x]][1]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
      #   move[[2]]=list()
      #   move[[2]][[1]]='R'
      #   move[[2]][[2]]=x
      #   move[[2]][[3]]=1
      #   
      #   out_list=c(out_list,list(move))
      #   eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==x]$index) +1)
      #   
      #   out_energy=c(out_energy,eval_out)
      # }
    }
  }
  
  for(r in c('A','B','C','D')){
    move=list()
    
    if(any(room_slots[[r]]!='') &! all(room_slots[[r]]%in%c('',r))){
        room_ind=min(which(room_slots[[r]]!=''))
        x=room_slots[[r]][room_ind]
         
        move[[1]]=list()
        move[[1]][[1]]='R'
        move[[1]][[2]]=r
        move[[1]][[3]]=room_ind
        
          for(i in 1:7){
            inbetween_hallway_pos=get_inbetween_pos(i,r)
            if(wait_slots[i]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
              move[[2]]=list()
              move[[2]][[1]]='H'
              move[[2]][[2]]=i

              out_list=c(out_list,list(move))
              eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +room_ind)
              out_energy=c(out_energy,eval_out)
            }
          }

        }
      
    }
    # if(room_slots[[r]][1]!=''&&!(room_slots[[r]][1]==r&&room_slots[[r]][2]==r) ){
    #   x=room_slots[[r]][1]
    #   
    #   move[[1]]=list()
    #   move[[1]][[1]]='R'
    #   move[[1]][[2]]=r
    #   move[[1]][[3]]=1
    #   
    #   for(i in 1:7){
    #     inbetween_hallway_pos=get_inbetween_pos(i,r)
    #     if(wait_slots[i]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
    #       move[[2]]=list()
    #       move[[2]][[1]]='H'
    #       move[[2]][[2]]=i
    #       
    #       out_list=c(out_list,list(move))
    #       eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +1)
    #       out_energy=c(out_energy,eval_out)
    #     }
    #   }
    #   
    # }
    # 
    # if(room_slots[[r]][1]=='' &&room_slots[[r]][2]!='' &&room_slots[[r]][2]!=r){
    #   x=room_slots[[r]][2]
    #   
    #   move[[1]]=list()
    #   move[[1]][[1]]='R'
    #   move[[1]][[2]]=r
    #   move[[1]][[3]]=2
    #   
    #   for(i in 1:7){
    #     inbetween_hallway_pos=get_inbetween_pos(i,r)
    #     if(wait_slots[i]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
    #       move[[2]]=list()
    #       move[[2]][[1]]='H'
    #       move[[2]][[2]]=i
    #       
    #       out_list=c(out_list,list(move))
    #       eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +2)
    #       out_energy=c(out_energy,eval_out)
    #     }
    #   }
      
  #   }
  #   
  # }
  
  return(list(moves=out_list,energy=out_energy))
}







####################################
get_energy_heuristic=function(wait_slots,room_slots){
  nrg_hall=sum((abs(hpos_index[as.character(which(wait_slots!=''))]-hpos_index[wait_slots[wait_slots!='']])+1)*energy_vals[wait_slots[wait_slots!='']])
  nrg_room=0
  for(r in c('A','B','C','D')){
    nrg_room=nrg_room+sum((abs(hpos_index[room_slots[[r]][!(room_slots[[r]]%in% c(r,''))]]-hpos_index[r])+2+(which(!(room_slots[[r]]%in% c(r,'')))-1))*energy_vals[room_slots[[r]][!(room_slots[[r]]%in% c(r,''))]])
  }
  
  nrg_heuristic=nrg_hall+nrg_room
  return(nrg_heuristic)
}




wait_slots=c('A','A','D','','','A','D')
room_slots=list('A'=c('','','','A'),'B'=c('B','B','B','B'),'C'=c('C','C','C','C'),'D'=c('','','D','D'))
find_best=function(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0){
  key=paste0(c(
    paste0(wait_slots,collapse='_')
    ,paste0(room_slots[['A']],collapse='_')
    ,paste0(room_slots[['B']],collapse='_')
    ,paste0(room_slots[['C']],collapse='_')
    ,paste0(room_slots[['D']],collapse='_')
  ),collapse='|')
  
  if(key %in% names(memoise_list)){
  # if(key %in% memoise_list$k){
    return(memoise_list[[key]]+energy)
    # return(memoise_list[k==key,]$val+energy)
  }
  
  if(all(wait_slots=='') &&all(room_slots[['A']] =='A') &&all(room_slots[['B']] =='B')&&all(room_slots[['C']] =='C')&&all(room_slots[['D']] =='D')  ){
    return(energy)
  }
  

  combo_out=list_legal_moves(wait_slots,room_slots)
  moves=combo_out$moves
  move_energies=combo_out$energy
  
  
  if(length(moves)==0){
    memoise_list[[key]]<<- Inf
    # memoise_list<<-rbind(memoise_list,data.table(k=key,val=Inf),use.names=T,fill=T)
    return(Inf)
  }
  # imp_state=impossible_state(moves,wait_slots,room_slots)
  # if(imp_state){
  #   memoise_list[[key]]<<- Inf
  #   # memoise_list<<-rbind(memoise_list,data.table(k=key,val=Inf),use.names=T,fill=T)
  #   return(Inf)
  # }
  
  out_energies=c()
  for(i in 1:length(moves)){
    m=moves[[i]]
    new_w_r_combo=exec_move(m,wait_slots,room_slots)
    out_energies=c(out_energies,get_energy_heuristic(new_w_r_combo$wait_slots,new_w_r_combo$room_slots)+move_energies[i])
  }

  # moves=moves[order(move_energies)]
  # move_energies=move_energies[order(move_energies)]
  # out_energies=out_energies[order(move_energies)]
  moves=moves[order(out_energies)]
  move_energies=move_energies[order(out_energies)]
  out_energies=out_energies[order(out_energies)]

  max_ener_cap=max_energy
  new_energy=Inf
  out_energy_final=Inf
  for(i in 1:length(moves)){
    
    m=moves[[i]]
    new_w_r_combo=exec_move(m,wait_slots,room_slots)
    
    if((out_energies[i]) <max_ener_cap){
      
      # print(i)
      out_energy=find_best(new_w_r_combo$wait_slots,new_w_r_combo$room_slots,max_energy=max_ener_cap-move_energies[i],path=c(),energy=energy+move_energies[i])
      
      # if(out_energy<out_energies[i]){
      #   print(out_energy)
      #   print(out_energies[i])
      #   print(wait_slots)
      #   print(room_slots)
      # }
      out_energy_final=min(out_energy_final,out_energy)
      # print(new_energy)
      max_ener_cap=min(max_ener_cap,out_energy-energy)
      # print(max_ener_cap)
    }
  }
  memoise_list[[key]]<<-out_energy_final-energy
  # memoise_list<<-rbind(memoise_list,data.table(k=key,val=out_energy_final-energy),use.names=T,fill=T)
  
  return(out_energy_final)
}
# microbenchmark(find_best(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0),times=5L)

wait_slots=c('','','','','','','')
room_slots=list('A'=c('B','A'),'B'=c('C','D'),'C'=c('B','C'),'D'=c('D','A'))

# wait_slots=c('','','','D','','','')
# room_slots=list('A'=c('B','A'),'B'=c('','B'),'C'=c('C','C'),'D'=c('D','A'))


# wait_slots=c('','','B','D','','','')
# room_slots=list('A'=c('B','A'),'B'=c('',''),'C'=c('C','C'),'D'=c('D','A'))

# wait_slots=c('','','B','','','','')
# room_slots=list('A'=c('B','A'),'B'=c('','D'),'C'=c('C','C'),'D'=c('D','A'))


# energy_vals[wait_slots[wait_slots!='']]
  
# wait_slots=c('','','','D','D','A','')
# room_slots=list('A'=c('','A'),'B'=c('B','B'),'C'=c('C','C'),'D'=c('',''))
# 
# 
# 
# wait_slots=c('','','','D','','','')
# room_slots=list('A'=c('B','A'),'B'=c('','B'),'C'=c('C','C'),'D'=c('D','A'))

wait_slots=rep('',7)
room_slots=list('A'=c('B','B'),'B'=c('C','C'),'C'=c('A','D'),'D'=c('D','A'))


t0=Sys.time()
memoise_list<<-list()
# memoise_list<<-data.table(k='blah',val=-1)
# setkey(memoise_list,key)
# pre_initiate(max_depth=6)

wait_slots=c('','','','','','','')
room_slots=list('A'=c('B','A'),'B'=c('C','D'),'C'=c('B','C'),'D'=c('D','A'))
test=find_best(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0)
Sys.time()-t0
# 




#######################################################################################3
#23.1 ~2min
# Rprof(filename="/home/eqiu/code_projects/aoc_2021/rprof.out")
t0=Sys.time()
# wait_slots=c('','','','','','C','C')
# room_slots=list('A'=c('',''),'B'=c('B','B'),'C'=c('A','D'),'D'=c('D','A'))
# test=find_best(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0)
# 
# wait_slots=c('','','','','','C','C')
# room_slots=list('A'=c('B','B'),'B'=c('',''),'C'=c('A','D'),'D'=c('D','A'))
# test=find_best(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0)

memoise_list<<-list()
wait_slots=rep('',7)
room_slots=list('A'=c('B','B'),'B'=c('C','C'),'C'=c('A','D'),'D'=c('D','A'))
test=find_best(wait_slots,room_slots,max_energy=13018+700*2+50*2,path=c(),energy=0)
Sys.time()-t0
# Rprof(NULL)
# summaryRprof("/home/eqiu/code_projects/aoc_2021/rprof.out")
# 20:27



#23.2 ~15min
t0=Sys.time()
memoise_list<<-list()
wait_slots=rep('',7)
# room_slots=list('A'=c('B','D','D','A'),'B'=c('C','C','B','D'),'C'=c('B','B','A','C'),'D'=c('D','A','C','A'))
room_slots=list('A'=c('B','D','D','B'),'B'=c('C','C','B','C'),'C'=c('A','B','A','D'),'D'=c('D','A','C','A'))
test=find_best(wait_slots,room_slots,max_energy=Inf,path=c(),energy=0)

Sys.time()-t0

############################################################3

# list_legal_rev_moves=function(wait_slots,room_slots){
#   out_list=list()
#   out_energy=c()
#   for(i in 1:7){
#     move=list()
#     if(wait_slots[i]!=''){
#       x=wait_slots[i]
#       
#       move[[1]]=list()
#       move[[1]][[1]]='H'
#       move[[1]][[2]]=i
#       
#       for(r in setdiff(c('A','B','C','D'),x)){
#         inbetween_hallway_pos=get_inbetween_pos(i,r)
#         
#         if(all(room_slots[[r]]==rep('',2)) && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]=='')) ){
#           move[[2]]=list()
#           move[[2]][[1]]='R'
#           move[[2]][[2]]=r
#           move[[2]][[3]]=2
#           
#           out_list=c(out_list,list(move))
#           eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +2)
#           
#           out_energy=c(out_energy,eval_out)
#         }
#         
#         if(room_slots[[r]][2]!='' && room_slots[[r]][1]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
#           move[[2]]=list()
#           move[[2]][[1]]='R'
#           move[[2]][[2]]=r
#           move[[2]][[3]]=1
#           
#           out_list=c(out_list,list(move))
#           eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +1)
#           
#           out_energy=c(out_energy,eval_out)
#         }
#       }
#     }
#   }
#   
#   for(r in c('A','B','C','D')){
#     move=list()
#     if(room_slots[[r]][1]==r ){
#       x=room_slots[[r]][1]
#       
#       move[[1]]=list()
#       move[[1]][[1]]='R'
#       move[[1]][[2]]=r
#       move[[1]][[3]]=1
#       
#       for(i in 1:7){
#         inbetween_hallway_pos=get_inbetween_pos(i,r)
#         if(wait_slots[i]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
#           move[[2]]=list()
#           move[[2]][[1]]='H'
#           move[[2]][[2]]=i
#           
#           out_list=c(out_list,list(move))
#           eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +1)
#           out_energy=c(out_energy,eval_out)
#         }
#       }
#       
#     }
#     
#     if(room_slots[[r]][1]=='' &&room_slots[[r]][2]==r){
#       x=room_slots[[r]][2]
#       
#       move[[1]]=list()
#       move[[1]][[1]]='R'
#       move[[1]][[2]]=r
#       move[[1]][[3]]=2
#       
#       for(i in 1:7){
#         inbetween_hallway_pos=get_inbetween_pos(i,r)
#         if(wait_slots[i]=='' && (length(inbetween_hallway_pos)==0 ||all(wait_slots[inbetween_hallway_pos]==''))){
#           move[[2]]=list()
#           move[[2]][[1]]='H'
#           move[[2]][[2]]=i
#           
#           out_list=c(out_list,list(move))
#           eval_out=energy_vals[x]*( abs(hallway_pos[name==i]$index-hallway_pos[name==r]$index) +2)
#           out_energy=c(out_energy,eval_out)
#         }
#       }
#       
#     }
#     
#   }
#   
#   return(list(moves=out_list,energy=out_energy))
# }
# 
# 
# wait_slots=c('','','A','','B','','')
# room_slots=list('A'=c('','A'),'B'=c('','B'),'C'=c('C','C'),'D'=c('D','D'))
# test=list_legal_rev_moves(wait_slots,room_slots)


# pre_initiate=function(wait_slots=rep('',7),room_slots=list('A'=c('A','A'),'B'=c('B','B'),'C'=c('C','C'),'D'=c('D','D')),max_depth=6,depth=0){
#   if(depth>max_depth){
#     return(0)
#   }
#   
#   key=paste0(c(
#     paste0(wait_slots,collapse='_')
#     ,paste0(room_slots[['A']],collapse='_')
#     ,paste0(room_slots[['B']],collapse='_')
#     ,paste0(room_slots[['C']],collapse='_')
#     ,paste0(room_slots[['D']],collapse='_')
#   ),collapse='|')
#   
#   move_list=list_legal_rev_moves(wait_slots,room_slots)
#   moves=move_list$moves
#   move_energies=move_list$energy
#   for(i in 1:length(move_list)){
#     m=moves[[i]]
#     new_w_r_combo=exec_move(m,wait_slots,room_slots)
#     out_energy=find_best(new_w_r_combo$wait_slots,new_w_r_combo$room_slots,max_energy=Inf,path=c(),energy=0)
#   }
#   
#   for(i in 1:length(move_list)){
#     m=moves[[i]]
#     new_w_r_combo=exec_move(m,wait_slots,room_slots)
#     pre_initiate(new_w_r_combo$wait_slots,new_w_r_combo$room_slots,max_depth=max_depth,depth=depth+1)
#   }
#   
#   return(0)
# }




# impossible_state=function(moves,wait_slots,room_slots){
#   possib_blockers=wait_slots[3:5]
#   possib_block_ind=which(possib_blockers!='')
# 
#   if(length(possib_blockers)==0){
#     return(F)
#   }
# 
#   unblockable=unlist(lapply(moves,function(x) x[[1]][[2]]))[unlist(lapply(moves,function(x) x[[1]][[1]]))=='H']
#   movable_rooms=unique(unlist(lapply(moves,function(x) x[[1]][[2]]))[unlist(lapply(moves,function(x) x[[1]][[1]]))=='R'])
#   
#   for(ind in possib_block_ind){
#     if(possib_blockers[ind] %in%wait_slots[as.numeric(unblockable)]){
#       next
#     }
# 
#     if(possib_blockers[ind] %in%movable_rooms){
#       next
#     }
# 
#     if(ind==1){
#       if(any(room_slots[['A']] %in% c('B','C','D'))){
#         return(T)
#       }
#       if(any(c(room_slots[['C']],room_slots[['B']],room_slots[['D']])) %in% c('A')){
#         return(T)
#       }
#     }
#     if(ind==2){
#       if(any(c(room_slots[['A']],room_slots[['B']]) %in% c('C','D'))){
#         return(T)
#       }
#       if(any(c(room_slots[['C']],room_slots[['D']]) %in% c('A','B'))){
#         return(T)
#       }
#     }
#     if(ind==3){
#       if(any(room_slots[['D']] %in% c('B','C','A'))){
#         return(T)
#       }
#       if(any(c(room_slots[['C']],room_slots[['B']],room_slots[['A']]) %in% c('D'))){
#         return(T)
#       }
#     }
# 
#   }
# 
#   return(F)
# }

# wait_slots=c('','','','A','','','')
# room_slots=list('A'=c('C','C'),'B'=c('D','D'),'C'=c('B','B'),'D'=c('','A'))
# combo_out=list_legal_moves(wait_slots,room_slots)
# moves=combo_out$moves
# test=impossible_state(moves,wait_slots,room_slots)