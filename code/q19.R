library(data.table)
# instrs=fread('/home/eqiu/code_projects/aoc_2021/data/input_19',header=F)

beacons=fread('/home/eqiu/code_projects/aoc_2021/data/input_19',header=F,fill=T)


beacons[,new_beac:=0]
beacons[substr(V1,1,3)=='---',new_beac:=1]
beacons[,beac_num:=cumsum(new_beac)-1]


beac2=beacons[is.finite(as.numeric(substr(V1,nchar(V1),nchar(V1)))),.(V1,beac_num)]

beacs3=beac2[,tstrsplit(V1,split=',')]
beacs3[,beac_num:=beac2$beac_num]
# storage.mode(beacs3)='numeric'

beacs3


#19.1
##list all permuates
permutes=as.matrix(expand.grid(3:1,3:1,3:1))
permutes=permutes[rowSums(permutes)==6,]
permutes=permutes[c(1:3,5:7),]
base_rots=diag(3)[t(permutes),]
reflects=list(  
  diag(c(1,1,1)),
  diag(c(-1,-1,-1)),
  
  diag(c(1,-1,-1)),
  diag(c(-1,1,-1)),
  diag(c(-1,-1,1)),
  
  diag(c(-1,1,1)),
  diag(c(1,-1,1)),
  diag(c(1,1,-1))
)
permute_mat=do.call(rbind,lapply(reflects,function(x) base_rots %*% x))

#dists function
get_dists=function(beacs){
  out_list=list()
  out_list=lapply(1:nrow(beacs),function(x) sweep(beacs,2,beacs[x,]))
  
  return(out_list)
}


#scanner matching
match_scanners=function(s1_beacs,s2_beacs){
  s1_dists=get_dists(s1_beacs)
  s2_dists=get_dists(s2_beacs)
  
  dists1=lapply(s1_dists,function(x) rowSums(abs(x)))
  dists2=lapply(s2_dists,function(x) rowSums(abs(x)))
  
  index_lookup=data.table(u=1:length(dists1),v=rep(1:length(dists2),each=length(dists1)))
  possible_matches=mapply(function(x,y) sum(dists1[[x]] %in%dists2[[y]]) >=12,1:length(dists1),rep(1:length(dists2),each=length(dists1)))
  test1=sum(possible_matches)
  
  # dists1=lapply(s1_dists,function(x) rowSums(abs(x^2)))
  # dists2=lapply(s2_dists,function(x) rowSums(abs(x^2)))
  # test2=sum(mapply(function(x,y) sum((dists1[[x]]%in%dists2[[y]]))>=12,1:length(dists1),rep(1:length(dists2),each=length(dists1))))
  
  s1_dists[[4]]
  s2_dists[[1]]
  
  if(test1<12){
    return(list(is_match=F))
  }
  
  
  possib_uv=index_lookup[(possible_matches),]
  # s2_dists_temp=list()
  for(i in 1:48){
    perm_mat=permute_mat[(3*(i-1)+1):(3*i),]
    sim_vec=list()
    if(det(perm_mat)==-1){
      next
    }
    
    for(q in 1:nrow(possib_uv)){
      u=possib_uv[q,]$u
      v=possib_uv[q,]$v
      M1=setkey(data.table(s1_dists[[u]]))
      M2=setkey(data.table(s2_dists[[v]]%*%perm_mat))
      
      n_same=sum(!is.na(M1[M2,which=T]))
      
      if(n_same>=12){
        sim_vec[[as.character(u)]]=c(u,v)
        n_max=n_same
      }
    }
    if(length(sim_vec)>=12){
      # print('hooray')
      return(list(sim_vec=sim_vec,n_same=length(sim_vec),perm_mat=perm_mat,is_match=T))
    }
    
    # for(u in 1:length(s1_dists)){
    #   if((length(sim_vec)+length(s1_dists)-u+1) <12){
    #     next
    #   }
    #   n_max=0
    #   for(v in 1:length(s2_dists)){
    #     
    #   }
    #   
    #   
    # }
    
  }
  
  
  return(list(is_match=F))
}

# intersect_dists

##############
# beacsx_0=t(matrix(c(
# 404,-588,-901
# ,528,-643,409
# ,-838,591,734
# ,390,-675,-793
# ,-537,-823,-458
# ,-485,-357,347
# ,-345,-311,381
# ,-661,-816,-575
# ,-876,649,763
# ,-618,-824,-621
# ,553,345,-567
# ,474,580,667
# ,-447,-329,318
# ,-584,868,-557
# ,544,-627,-890
# ,564,392,-477
# ,455,729,728
# ,-892,524,684
# ,-689,845,-530
# ,423,-701,434
# ,7,-33,-71
# ,630,319,-379
# ,443,580,662
# ,-789,900,-551
# ,459,-707,401
# ),nrow=3))
# 
# beacsx_1=t(matrix(c(
#   686,422,578
#   ,605,423,415
#   ,515,917,-361
#   ,-336,658,858
#   ,95,138,22
#   ,-476,619,847
#   ,-340,-569,-846
#   ,567,-361,727
#   ,-460,603,-452
#   ,669,-402,600
#   ,729,430,532
#   ,-500,-761,534
#   ,-322,571,750
#   ,-466,-666,-811
#   ,-429,-592,574
#   ,-355,545,-477
#   ,703,-491,-529
#   ,-328,-685,520
#   ,413,935,-424
#   ,-391,539,-444
#   ,586,-435,557
#   ,-364,-763,-893
#   ,807,-499,-711
#   ,755,-354,-619
#   ,553,889,-390
# ),nrow=3))

#find matches
match_list=list()
n_overlap=0
for(i in 0:(max(beacs3$beac_num)-1)){
  for(j in (i+1):max(beacs3$beac_num)){
    print(paste0(i,',',j))
    if(i==j){next}
      beacsx_0=as.matrix(beacs3[beac_num==i,.(as.numeric(V1),as.numeric(V2),as.numeric(V3))])
      beacsx_1=as.matrix(beacs3[beac_num==j,.(as.numeric(V1),as.numeric(V2),as.numeric(V3))])
      test=match_scanners(beacsx_0,beacsx_1)
      if(test$is_match){
        n_overlap=n_overlap+test$n_same
        print('match')
        match_list[[paste0(i,'_',j)]]=test
      }
  }
}

#map beacons using scanner matches
to_map_list=names(match_list)
zero_map=list()
zero_map[['0']]=list(mat=diag(3),trans=matrix(c(0,0,0),ncol=3))
out_beac_list=data.table(beacs3[beac_num==0,.(as.numeric(V1),as.numeric(V2),as.numeric(V3))])
out_beac_list

max_m_dist=0
while(length(to_map_list)>0){
  n=to_map_list[1]
  print(n)
  s1=gsub('(.*)_(.*)','\\1',n)
  s2=gsub('(.*)_(.*)','\\2',n)
  
  if(!(s1 %in% names(zero_map))){
    
    if(!(s2 %in% names(zero_map))){
      to_map_list=c(to_map_list[-1],to_map_list[1])
      next
    }
    new_n=paste0(s2,'_',s1)
    match_list[[new_n]]=list(sim_vec=lapply(match_list[[n]]$sim_vec,function(x) x[2:1]),perm_mat=solve(match_list[[n]]$perm_mat))
    match_list[[n]]=NULL
    n=new_n
    s1=gsub('(.*)_(.*)','\\1',n)
    s2=gsub('(.*)_(.*)','\\2',n)
  }  
  beacsx_0=as.matrix(beacs3[beac_num==as.numeric(s1),.(as.numeric(V1),as.numeric(V2),as.numeric(V3))])
  beacsx_1=as.matrix(beacs3[beac_num==as.numeric(s2),.(as.numeric(V1),as.numeric(V2),as.numeric(V3))])
  

  beacsx_1%*%match_list[[n]]$perm_mat
  
  beacsx_1[match_list[[n]]$sim_vec[[1]][2],]%*%match_list[[n]]$perm_mat+trans_rel
  
  trans_rel=beacsx_0[match_list[[n]]$sim_vec[[1]][1],]-beacsx_1[match_list[[n]]$sim_vec[[1]][2],]%*%match_list[[n]]$perm_mat
  
  
  
  zero_map[[s2]]=list(mat=match_list[[n]]$perm_mat%*%zero_map[[s1]]$mat,trans=trans_rel%*%zero_map[[s1]]$mat+zero_map[[s1]]$trans)
  

  
  s2_beac_rel_list=data.table(sweep(beacsx_1%*%zero_map[[s2]]$mat,2,-zero_map[[s2]]$trans))
  # s2_beac_rel_list
  
  
  out_beac_list=rbind(out_beac_list,s2_beac_rel_list)
  out_beac_list=unique(out_beac_list)
  print(nrow(out_beac_list))
  
  to_map_list=to_map_list[-1]
}

#19.2
scanner_locs=do.call(rbind,lapply(zero_map,function(x) x$trans))

scanner_dists=get_dists(scanner_locs)

max(apply(abs(do.call(rbind, scanner_dists)),1,sum))
