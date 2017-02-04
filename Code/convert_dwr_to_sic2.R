library(dplyr)
library(tidyr)
library(tibble)
library(readr)


setwd("/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities/")

naicssic<-read_csv("data/NAICS_SIC.csv")%>%
  dplyr::select(NAICS_Code,SIC_Code)%>%
  mutate(
    naics=as.character(NAICS_Code%/%100)
  )

dwr<-read_csv("data/dwrNaics.csv")

comp_sic_list<-read_csv("data/compustat_sic.csv")%>%
  dplyr::select(sic)%>%
  unique()%>%
  mutate(
    isThere=1
  )

sic_listen<-naicssic%>%left_join(dwr)%>%
  mutate(
    sic_2d=100*SIC_Code%/%100
  )%>%
  rename(sic=SIC_Code)%>%
  left_join(comp_sic_list)



#sic_listen<-merged%>%left_join(comp_sic_list)

adj_dwr<-sic_listen%>%
  filter(!is.na(isThere),!is.na(dwr))%>%
  group_by(yyyyq,sic)%>%
  summarise(
    dwrAdj=mean(dwr)
  )

av_dwr<-sic_listen%>%
  filter(is.na(isThere),!is.na(dwr),!is.na(sic_2d))%>%
  group_by(yyyyq,sic_2d)%>%
  summarise(
    dwrAv=mean(dwr)
  )


sic_final<-sic_listen%>%
  left_join(av_dwr)%>%
  left_join(adj_dwr)%>%
  mutate(
    dwr_final=ifelse(is.na(isThere),dwrAv,dwrAdj),
    sic_final=ifelse(is.na(isThere),sic_2d,sic)
  )%>%
  dplyr::select(yyyyq,sic_final,dwr_final)%>%
  rename(
    sic=sic_final,
    dwr=dwr_final
  )%>%
  unique()




dwrSic<-comp_sic_list%>%left_join(sic_final)%>%
  mutate(
    sic=ifelse(is.na(yyyyq),100*(sic%/%100),sic) # Dette var jo en mye mer elegant måte å gjøre ting på...
  )%>%dplyr::select(sic)%>%
  unique()%>%
  left_join(sic_final)%>%
  filter(!is.na(yyyyq))


write_csv(dwrSic,"data/dwrSic.csv")
