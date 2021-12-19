library(data.table)
hex=fread('/home/eqiu/code_projects/aoc_2021/data/input_16',header=F)
hex2=unlist(strsplit(hex$V1,split=''))

hex_to_binary=function(hex){
  switch(hex,
    '0'='0000',
    '1'='0001',
    '2'='0010',
    '3'='0011',
    '4'='0100',
    '5'='0101',
    '6'='0110',
    '7'='0111',
    '8'='1000',
    '9'='1001',
    'A'='1010',
    'B'='1011',
    'C'='1100',
    'D'='1101',
    'E'='1110',
    'F'='1111'
  )
}

parse_literal_packet=function(packet){
  if(nchar(packet)==0|as.numeric(packet)==0){
    return(0)
  }
  
  version=substr(packet,1,3)
  packet_id=substr(packet,4,6)
  
  # print(packet)
  # print(version)
  
  if( packet_id=='100'){#literal
    index=7
    packet_lead=substr(packet,index,index)
    while(packet_lead!='0'){
      index=index+5
      packet_lead=substr(packet,index,index)
    }
    new_packet=substr(packet,index+5,nchar(packet))
    out_val=parse_literal_packet(new_packet)
    return(c(out_val,strtoi(version,base=2L)))
  }else{#operator
    length_type=substr(packet,7,7)
    if(length_type=='0'){
      bit_length=strtoi(substr(packet,8,22),base=2L)
      
      new_packet=substr(packet,23,nchar(packet))
    }else if(length_type=='1'){
      num_packets=strtoi(substr(packet,8,18),base=2L)
      new_packet=substr(packet,19,nchar(packet))
    }
    out_val=parse_literal_packet(new_packet)
    return(c(out_val,strtoi(version,base=2L)))
  }
}

h2b=Vectorize(hex_to_binary)
h2b2=function(hex){paste0(h2b(unlist(strsplit(hex,split=''))),collapse='')}


# bin_str=paste0(h2b(hex2),collapse='')

sum(parse_literal_packet(h2b2(hex$V1)))


#16.2
find_packet_end=function(packet){
  if(nchar(packet)==0|as.numeric(packet)==0){
    return(0)
  }
  version=substr(packet,1,3)
  packet_id=substr(packet,4,6)
  if( packet_id=='100'){#literal
    index=7
    packet_lead=substr(packet,index,index)
    while(packet_lead!='0'){
      index=index+5
      packet_lead=substr(packet,index,index)
    }
    return(index+5)
  }else{#operator
    length_type=substr(packet,7,7)
    if(length_type=='0'){
      bit_length=strtoi(substr(packet,8,22),base=2L)
      return(23+bit_length)
    }else if(length_type=='1'){
      num_packets=strtoi(substr(packet,8,18),base=2L)
      subp_lengths=0
      n_packs=0
      
      new_packet=substr(packet,19,nchar(packet))
      while(n_packs<num_packets){
        n_add=find_packet_end(new_packet)
        new_packet=substr(new_packet,n_add,nchar(new_packet))
        subp_lengths=subp_lengths+n_add-1
        n_packs=n_packs+1
      }
      return(19+subp_lengths)
    }
  }
}


find_packet_end(h2b2('A0016C880162017C3686B18A3D4780'))
convert <- function(x) {
  y <- as.numeric(strsplit(x, "")[[1]])
  sum(y * 2^rev((seq_along(y)-1)))
}

parse_packet_val=function(packet){
  if(nchar(packet)==0|as.numeric(packet)==0){
    return(0)
  }
  
  # version=substr(packet,1,3)
  packet_id=substr(packet,4,6)
  if( packet_id=='100'){#literal
    index=7
    packet_lead=substr(packet,index,index)
    packet_str=substr(packet,index+1,index+4)
    while(packet_lead!='0'){
      index=index+5
      packet_str=paste0(packet_str,substr(packet,index+1,index+4))
      packet_lead=substr(packet,index,index)
    }
    # if(!is.finite(strtoi(packet_str,base=2L))){
    #   print(packet_str)
    # }
    # return(strtoi(packet_str,base=2L))
    return(convert(packet_str))
    
  }else{#operator
    length_type=substr(packet,7,7)
    if(length_type=='0'){
      bit_length=strtoi(substr(packet,8,22),base=2L)
      
      sub_packet=substr(packet,23,23+bit_length-1)
      index=0
      sub_packet_vals=c()
      while(index<bit_length){
        next_start=find_packet_end(sub_packet)
        
        sub_packet_vals=c(sub_packet_vals,parse_packet_val(substr(sub_packet,1,next_start-1)))
        
        sub_packet=substr(sub_packet,next_start,nchar(sub_packet))
        index=index+next_start
      }
    }else if(length_type=='1'){
      num_packets=strtoi(substr(packet,8,18),base=2L)
      sub_packet=substr(packet,19,nchar(packet))
      
      n_packs=0
      sub_packet_vals=c()
      while(n_packs<num_packets){
        next_start=find_packet_end(sub_packet)
        sub_packet_vals=c(sub_packet_vals,parse_packet_val(substr(sub_packet,1,next_start-1)))
        
        sub_packet=substr(sub_packet,next_start,nchar(sub_packet))
        n_packs=n_packs+1
      }
    }
    
    if(packet_id=='000'){pack_val=(sum(sub_packet_vals))}
    if(packet_id=='001'){pack_val=(prod(sub_packet_vals))}
    if(packet_id=='010'){pack_val=(min(sub_packet_vals))}
    if(packet_id=='011'){pack_val=(max(sub_packet_vals))}
    if(packet_id=='101'){pack_val=(as.numeric(sub_packet_vals[1]>sub_packet_vals[2]))}
    if(packet_id=='110'){pack_val=(as.numeric(sub_packet_vals[1]<sub_packet_vals[2]))}
    if(packet_id=='111'){pack_val=(as.numeric(sub_packet_vals[1]==sub_packet_vals[2]))}
    # print(sub_packet_vals)
    # print(pack_val)
    # if(!is.finite(pack_val)){
    #   print(sub_packet_vals)
    # }
    return(pack_val)
  }
}
# parse_packet_val(h2b2('C200B40A82'))
# parse_packet_val(h2b2('04005AC33890'))
# parse_packet_val(h2b2('880086C3E88112'))
# parse_packet_val(h2b2('CE00C43D881120'))
# parse_packet_val(h2b2('D8005AC2A8F0'))
# parse_packet_val(h2b2('F600BC2D8F'))
# parse_packet_val(h2b2('9C005AC2F8F0'))
# parse_packet_val(h2b2('9C0141080250320F1802104A08'))
parse_packet_val(h2b2(hex$V1))
