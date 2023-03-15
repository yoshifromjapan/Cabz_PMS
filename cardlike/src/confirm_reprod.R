library(tidyverse)
library(here)
library(ProjectTemplate)
library(haven)
library(skimr)
library(lubridate)
library(janitor)
library(gtsummary)

create.project(project.name = "cardlike",template = "minimal")

jet.med.path<-here("cardlike","data","jetmed.sas7bdat")

jet.med<-read_sas(
  data_file=jet.med.path,
  .name_repair = "unique"
)

saihi.path<-here("cardlike","data","saihi.sas7bdat")

saihi<-read_sas(
  data_file=saihi.path,
  .name_repair = "unique"
)

back.path<-here("cardlike","data","back.sas7bdat")

back<-read_sas(
  data_file=back.path,
  .name_repair = "unique"
) %>% mutate(patid=str_pad(.$PATID,width = 10,side="left", pad="0"))

pat.path<-here("cardlike","data","pat_cardlike")

pat<-read_csv(
  file=pat.path,
  name_repair = "unique"
) %>% mutate(patid=str_pad(.$PATID,width = 10,side="left", pad="0")) %>% select(patid)

saihi.r1 <-saihi %>% filter(SAF==1) %>% select("PATID") %>% mutate(patid=str_pad(.$PATID,width = 10,side="left", pad="0")) %>% select(patid) %>% inner_join(.,pat,by="patid")

jet.med.1<- inner_join(saihi,jet.med,by="PATID") %>% mutate(fst.date=ymd(FSTDATE),last.date=ymd(ENDDATE),trt.int=interval(fst.date,last.date),trt.dur=as.duration(trt.int),trt.dur.day=as.numeric(trt.dur, unit = "days")) %>% select(PATID,JETCYCLE,trt.dur.day,JETDOSEN) 

jet.med.2 <- jet.med.1 %>% group_by(PATID) %>% summarize(max.cyc=max(JETCYCLE),max.dur=max(trt.dur.day),cumdose=max(cumsum(JETDOSEN)))%>% clean_names()
skim(jet.med.2) 

back.1<-inner_join(back,saihi.r1,by="patid") %>% clean_names() %>% select("patid","fstdosen","fstdosec","fstdosec2","fstdate","enddate","obsterm","doses","dosecyc","rdi","prepsa_a","psaeff2_a","stopum","stop_ae") %>% inner_join(jet.med.2,by="patid") %>% mutate(trtgrp=if_else(fstdosen<=20,0,1))

skim(back.1)




back.1 %>% select(trtgrp,dosecyc,obsterm,doses, rdi, prepsa_a,psaeff2_a,stop_ae) %>% tbl_summary(by=trtgrp, 
                       label =list(dosecyc~"Trt Cycles",obsterm~"Duration(days)",doses~"Cumul dose", rdi~"Relative DI", prepsa_a~"Base PSA",psaeff2_a~"PSA resp",stop_ae~"Discont due to AE"),
                       percent="column",
                       digits=list(
                                   doses~1,
                                   rdi~1,
                                   prepsa_a~1),
                       statistic=list(dosecyc~"{median}({min}, {max})",
                                      obsterm~"{median}({min}, {max})",
                                      doses~"{median}({min}, {max})",
                                      rdi~"{median}({min}, {max})",
                                      prepsa_a~"{median}({min}, {max})",
                                      psaeff2_a~"{n}/{N} ({p}%)",
                                      stop_ae~"{n}/{N} ({p}%)"),
                       type=list(psaeff2_a~"categorical",
                                 stop_ae~"categorical")) %>% 
  add_n() %>% add_overall()









