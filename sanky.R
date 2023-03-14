library(magrittr)
library(dplyr)


Dox<-r_T_PCHED_170620 %>% dplyr::rename(stdt=PCHEDFDT) %>% dplyr::rename(eddt=PCHEDTDT) %>% dplyr::mutate(drugname="Dox") %>% dplyr::select(c(VERNO,PATID,PAGENO,RECORDNO,drugname,stdt,eddt))
hor<-r_T_PHOR2_170620 %>% dplyr::rename(stdt=PHOR2FDT) %>% dplyr::rename(eddt=PHOR2TDT) %>% dplyr::mutate(drugname=ifelse(PHOR2==1,"Enz",ifelse(PHOR2==2,"Abi","Oth"))) %>% dplyr::select(c(VERNO,PATID,PAGENO,RECORDNO,drugname,stdt,eddt))
jet<-r_T_JETMED_170620 %>% dplyr::filter(JETCYCLE==1) %>% dplyr::rename(stdt=JETFDT) %>% dplyr::mutate(drugname="Cabz") %>% dplyr::select(c(VERNO,PATID,PAGENO,RECORDNO,drugname,stdt))

library(tidyr)
library(stringr)
library(lubridate)
library(networkD3)
library(purrr)
library(data.table)

temp1<-dplyr::bind_rows(Dox,hor) %>% dplyr::filter(!stringr::str_detect(.$stdt,pattern="99") & !stringr::str_detect(.$eddt,pattern="99") ) %>% dplyr::arrange(PATID,stdt) %>% dplyr::mutate(stdate=lubridate::ymd(.$stdt),eddate=lubridate::ymd(.$eddt)) 

temp2<-jet %>% dplyr::filter(!stringr::str_detect(.$stdt,pattern="99")) %>% dplyr::mutate(stdate=lubridate::ymd(.$stdt)) %>% dplyr::bind_rows(temp1,jet) %>% dplyr::arrange(PATID,stdate) %>% dplyr::select(c(VERNO,PATID,PAGENO,RECORDNO,drugname,stdate,eddate)) %>% dplyr::group_by(PATID) %>% dplyr::mutate(lag_stdate = dplyr::lead(stdate, n = 1, default = NA)) %>% dplyr::mutate(line=min_rank(stdate)) %>% dplyr::mutate(drugname1=str_c(line,drugname,sep="_")) %>% dplyr::mutate(nextdrug = dplyr::lead(drugname1, n = 1, default = NA)) %>% dplyr::filter(drugname != "Cabz") %>% dplyr::filter(!is.na(nextdrug))
    
  temp3<-temp2 %>% group_by(drugname1,nextdrug) %>% summarise(n=n())
  # %>% dplyr::filter(stdate <= lag_stdate & lag_stdate < eddate) ?d?????Ă????͈̂ĊO?Ə??Ȃ??B70 row
  #temp3<-temp2 %>% tidyr::pivot_wider(id_cols=PATID, names_from=line,values_from=drugname1)
  nodes <- data.frame(
    name=c(as.character(temp3$drugname1), as.character(temp3$nextdrug)) %>% unique() 
  ) %>% dplyr::mutate(line=stringr::str_sub(name,1,1))
  nodes$linen<-as.numeric(nodes$line) 
  nodes<-nodes%>% dplyr::arrange(linen) %>% dplyr::select(name)
  map(nodes$name,str_sub,start=3,end=6)
  drugs<-unlist(map(nodes$name,str_sub,start=3,end=6))
  nodes$group<-as.factor(drugs)
  my_color <- 'd3.scaleOrdinal() .domain(["Abi", "Enz","Dox","Cabz"]) .range(["rgb(62,144,208)","rgb(190,0,107)", "rgb(0,165,144)","rgb(71,103,40)"])'
  
  
  
  temp3$IDsource<-match(temp3$drugname1, nodes$name)-1
  temp3$IDtarget <- match(temp3$nextdrug, nodes$name)-1
  
  txt <- temp3 %>% group_by(IDtarget) %>% summarize(total=sum(n)) %>% rename(ID=IDtarget)
  txt1 <- temp3 %>% group_by(IDsource) %>% summarize(total=sum(n)) %>% rename(ID=IDsource)
  txt2<-union(txt,txt1) %>% arrange(ID)
  
  nodes1<-nodes
  nodes1[txt2$ID+1L,]$name<-paste0(nodes1[txt2$ID+1L,]$name, ' (n=', txt2$total, ')')
  
  sankeyNetwork(Links=temp3, Nodes=nodes1, Source='IDsource', Target='IDtarget', 
                Value='n', NodeID='name',NodeGroup="group",
                colourScale = my_color,
                sinksRight = FALSE,fontSize = 16)
  
  write.table(temp3, file="data_example.csv", sep=",",row.names = FALSE)
  write.table(nodes1, file="node_example.csv", sep=",",row.names = FALSE)
  
  sankeyNetwork(Links=data_r1, Nodes=NODE, Source='IDsource', Target='IDtarget', 
                Value='n', NodeID='name',NodeGroup="group",
                colourScale = my_color,
                sinksRight = FALSE,fontSize = 16)
  
  sankeyNetwork(Links=DATA_CABZ, Nodes=NODE_CABZ, 
                Source='IDsource', Target='IDtarget', 
                Value='n', NodeID='name',NodeGroup="group",
                colourScale = my_color,
                sinksRight = FALSE,fontSize = 16)
  
  
  sankeyNetwork(Links=DATA_CABAZ, Nodes=NODE_CABAZ, 
                Source='IDsource', Target='IDtarget', 
                Value='n', NodeID='name',NodeGroup="group",
                colourScale = my_color,
                sinksRight = FALSE,fontSize = 16)
  
  