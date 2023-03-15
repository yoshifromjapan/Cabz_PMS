library(tidyverse)
library(here)
library(ProjectTemplate)
library(haven)
library(skimr)
library(lubridate)
library(janitor)
library(gtsummary)
library(gtreg)
library(flextable)

create.project(project.name = "cardlike",template = "minimal")

ae.path<-here("cardlike","data","ae_pcv.sas7bdat")

ae<-read_sas(
  data_file=ae.path,
  .name_repair = "unique"
) %>% clean_names() 


ae.s1<-ae %>% filter(saf==1,com_e_p_fg=="Y", aerl==2) %>% select(patid,com_e_pt_j, com_e_soc_j, aerl,aebeg, aebegday,ctcgr,jettr4,starts_with("aekbn"),starts_with("neut")) %>% mutate(patid=str_pad(.$patid,width = 10,side="left", pad="0"))

saihi.path<-here("cardlike","data","saihi.sas7bdat")

saihi<-read_sas(
  data_file=saihi.path,
  .name_repair = "unique")

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

ae.s2<- inner_join(saihi.r1,ae.s1,by="patid") %>% mutate(any_ae=TRUE,g3_ae=if_else(ctcgr>=3 & !is.na(ctcgr),TRUE,FALSE))　%>% mutate(aesi=case_when(
  aekbn1==1~"好中球減少症",
  aekbn2==1~"発熱性好中球減少症",
  aekbn3==1~"腎不全",
  aekbn4==1~"重篤な感染症",
  aekbn5==1~"貧血",
  aekbn6==1~"下痢",
  aekbn7==1~"ニューロパチー"
),
pt.r=case_when(
  com_e_pt_j %in% c("好中球減少症","無顆粒球症","好中球数減少")~"好中球減少症*",
  str_detect(com_e_pt_j,"血小板減少症|血小板数減少")~"血小板減少症*",
  str_detect(com_e_pt_j,"白血球減少症|白血球数減少")~"白血球減少症*",
  TRUE~com_e_pt_j
),
soc.r=case_when(
  str_detect(com_e_pt_j,"好中球減少症|無顆粒球症|好中球数減少")~"血液およびリンパ系障害",
  str_detect(com_e_pt_j,"血小板減少症|血小板数減少")~"血液およびリンパ系障害",
  str_detect(com_e_pt_j,"白血球減少症|白血球数減少")~"血液およびリンパ系障害",
  TRUE~com_e_soc_j
)
) %>% select(patid,aesi,pt.r,com_e_pt_j,soc.r,com_e_soc_j,any_ae,g3_ae)

ae.s2 |>
  tbl_ae_focus(
    id_df = saihi.r1,
    id = patid,
    ae = com_e_pt_j,
    soc =com_e_soc_j, 
    sort=c("ae","soc"),
    include = c(any_ae,g3_ae)
  ) 

ae.s2 |>
  tbl_ae_focus(
    id_df = saihi.r1,
    id = patid,
    ae = pt.r,
    soc =soc.r, 
    sort=c("ae","soc"),
    include = c(any_ae,g3_ae)
  ) 


adr.all<- inner_join(saihi.all,ae,by="patid") %>% mutate(any_ae=TRUE,g3_ae=if_else(ctcgr>=3 & !is.na(ctcgr),TRUE,FALSE)) %>% filter(com_e_p_fg=="Y", aerl==2) %>% mutate(aesi=case_when(
  aekbn1==1~"好中球減少症",
  aekbn2==1~"発熱性好中球減少症",
  aekbn3==1~"腎不全",
  aekbn4==1~"重篤な感染症",
  aekbn5==1~"貧血",
  aekbn6==1~"下痢",
  aekbn7==1~"ニューロパチー"
))

saihi.all<-saihi %>% filter(SAF==1) %>% clean_names() %>% select(patid)

# 有害事象を全例で出力
adr.out<-adr.all %>% tbl_ae_focus(
    id_df = saihi.all,
    id = patid,
    ae = com_e_pt_j,
    soc =com_e_soc_j, 
    sort=c("ae","soc"),
    include = c(any_ae,g3_ae)
  ) 
adr.path<-here("cardlike","src","adr.docx")

adr.out %>% as_flex_table() %>% flextable::save_as_docx(path=adr.path)

# 特定の有害事象を全例で出力
aesi.out<-adr.all %>% tbl_ae_focus(
  id_df = saihi.all,
  id = patid,
  ae = aesi,
  sort = "ae",
  include = c(any_ae,g3_ae)
) 

aesi.path<-here("cardlike","src","aesi.docx")

aesi.out %>% as_flex_table() %>% flextable::save_as_docx(path=aesi.path)



