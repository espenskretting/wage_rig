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

merged<-dwr%>%left_join(naicssic)%>%
  mutate(
    sic_2d=100*SIC_Code%/%100
  )

av_dwr<-merged%>%
  dplyr::select(-SIC_Code)%>%
  rename(
    sic2=sic_2d
  )%>%
  unique()%>%
  group_by(yyyyq,sic2)%>%
  summarise(
    dwrAv=mean(dwr)
  )

sic_data<-merged%>%dplyr::select(yyyyq,dwr,SIC_Code)%>%rename(sic=SIC_Code)

comp_sic_list<-read_csv("data/compustat_sic.csv")%>%
  dplyr::select(sic)%>%
  unique()

sic_list1<-comp_sic_list%>%left_join(sic_data)
sic_list2<-comp_sic_list%>%
  mutate(
    sic2=100*(sic%/%100)
    )%>%
  left_join(av_dwr)

dwrSic<-sic_list2%>%
  left_join(sic_list1)%>%
  mutate(
    dwr2=ifelse(is.na(dwr),dwrAv,dwr)
  )%>%
  dplyr::select(yyyyq,sic,dwr2)%>%
  rename(
    dwr=dwr2
  )

write_csv(dwrSic,"data/dwrSic.csv")




