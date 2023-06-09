library(tidyverse)
library(here)
library(ProjectTemplate)
library(haven)
library(skimr)
library(lubridate)
library(janitor)
library(gtsummary)
library(readxl)
library(survival)
library(survminer)

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

back.1<-inner_join(back,saihi.r1,by="patid") %>% clean_names() %>% select("patid","fstdosen","fstdosec","fstdosec2","fstdate","enddate","obsterm","doses","dosecyc","rdi","prepsa_a","psaeff2_a","stopum","stop_ae","ttf") %>% inner_join(jet.med.2,by="patid") %>% mutate(trtgrp=cut(fstdosen,breaks=c(-Inf,20,Inf), labels = c("x<=20","20<x")))

skim(back.1)

ae.path<-here("cardlike","data","ae_pcv.sas7bdat")

ae<-read_sas(
  data_file=ae.path,
  .name_repair = "unique"
) %>% clean_names() 


stop.ae<-ae %>% filter(saf==1, aerl==2,aeflg==1,jettr4==-1) %>% select(patid) %>% mutate(ae.stop.flg=1) %>% distinct()

back.2<-back.1 %>% left_join(stop.ae,by="patid")
  
back.out<-back.2 %>% select(trtgrp,dosecyc,obsterm,ttf,doses, rdi, prepsa_a,psaeff2_a,stop_ae,ae.stop.flg) %>% tbl_summary(by=trtgrp, 
                       label =list(dosecyc~"Trt Cycles",obsterm~"Duration(days)",ttf~"Time to failure",doses~"Cumul dose", rdi~"Relative DI", prepsa_a~"Base PSA",psaeff2_a~"PSA resp",stop_ae~"Discont due to AE",ae.stop.flg~"Discont. due to ADR"),
                       percent="column",
                       digits=list(
                                   doses~1,
                                   rdi~1,
                                   prepsa_a~1),
                       statistic=list(dosecyc~"{median}({min}, {max})",
                                      obsterm~"{median}({min}, {max})",
                                      ttf~"{median}({min}, {max})",
                                      doses~"{median}({min}, {max})",
                                      rdi~"{median}({min}, {max})",
                                      prepsa_a~"{median}({min}, {max})",
                                      psaeff2_a~"{n}/{N} ({p}%)",
                                      stop_ae~"{n}/{N} ({p}%)",
                                      ae.stop.flg~"{n}/{N} ({p}%)"
                                      ),
                       type=list(psaeff2_a~"categorical",
                                 stop_ae~"categorical",
                                 ae.stop.flg~"categorical")
                       ) %>% 
  add_n() %>% add_overall()

back.out.path<-here("cardlike","src","back_card.docx")

back.out %>% as_flex_table() %>% flextable::save_as_docx(path=back.out.path)


os.path<-here("cardlike","data","os_data286.xlsx")

os.data<-read_excel(
  path =os.path,
  sheet="OS_2nd_ARAT",
  skip=13,
  .name_repair = "unique"
) %>% mutate(patid=str_pad(.$patid,width = 7,side="left", pad="0"))

back.all<-back %>% clean_names() %>% select("patid","fstdosen","fstdosec","fstdosec2","fstdate","enddate","obsterm","doses","dosecyc","rdi","prepsa_a","psaeff2_a","stopum","stop_ae","ttf","os","os_csr") %>% inner_join(jet.med.2,by="patid") %>% mutate(trtgrp=cut(fstdosen,breaks=c(-Inf,20,Inf), labels = c("x<=20","20<x")))

os.data.2<-left_join(os.data,back.all,by="patid") %>% mutate(event=1-Censored)


fit<-survfit(Surv(OS_4L_CABZ,event)~1,data=os.data.2)
summary(fit)
ggsurvplot(fit, data = os.data.2, risk.table = TRUE)
surv_median(fit)







