library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(readr)
library(data.table)
#library(tempdisagg)



setwd("/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities")

state_names=list("al","ak","az","ar","ca","co","ct","de","dc","fl","ga","hi","id","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo","mt","ne","nv","nh","nj","nm","ny","nc","nd","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","vt","va","wa","wv","wi","wy")
stateFiles<-list()

for(states in 1:length(state_names)){
  print(state_names[states])
  data<-read_csv(paste("/media/espen/My Passport/QWI/utpakket/qwi_",state_names[states],"_sa_fs_gs_n4_op_u.csv", sep=""),col_types=cols(
    EarnS=col_double(),
    geography=col_integer()
    )
    )

  data2<-data%>%
    # making sure the earningsdata is available and removing unecessary categories:
    filter(sEmpS==1,sex!=0,firmsize!=0,agegrp!="A00",!is.na(EarnS))%>% 
    unite(yyyyq,year,quarter,sep="")%>%
    select(yyyyq,sex,agegrp,industry,firmsize,geography,EarnS)
  
  stateFiles[[states]]<-data2
  print(state_names[states])
}

THEdata<-dplyr::bind_rows(stateFiles)

write_csv(THEdata,"data/qwi_all_states.csv")
#rm(data,data2,stateFiles)
THEdata<-read_csv("data/qwi_all_states.csv")


################################################################################v
# For dwsFirmsize:
firmsizeData<-THEdata%>%
  unite(id,sex,agegrp,industry,geography)%>%
  arrange(firmsize,id,yyyyq)%>%
  mutate(
    wage_growth=ifelse(id==lag(id),EarnS/lag(EarnS)-1,NA)
  )%>%
  filter(!is.na(wage_growth),wage_growth>-0.5,wage_growth<5)%>%
  group_by(firmsize,yyyyq)%>%
  filter(n()>=10)%>% # Removing the time*firmsize observation if less than 10 observations
  ungroup()

time<-unique(firmsizeData$yyyyq)
firm_size<-unique(firmsizeData$firmsize)

dwrFirmsize<-matrix(nrow=length(time),ncol=length(firm_size),dimnames = list(time,firm_size))

for (firmsize_var in 1:length(firm_size)){ #length(firmsize)
  for (time_var in 1:length(time)){ #length(time)
    dat<-as.matrix(firmsizeData%>%filter(firmsize==firm_size[firmsize_var],yyyyq==time[time_var])%>%select(wage_growth))
    if(length(dat)>0){
      distr<-ecdf(dat)
      dwrFirmsize[time_var,firmsize_var]<-1-distr(2*median(dat))-distr(0)
    }
  }
}



################################################################################v
# For dwsState:
stateData<-THEdata%>%
  unite(id,sex,agegrp,firmsize,industry)%>%
  arrange(geography,id,yyyyq)%>%
  mutate(
    wage_growth=ifelse(id==lag(id),EarnS/lag(EarnS)-1,NA)
  )%>%
  filter(!is.na(wage_growth),wage_growth>-0.5,wage_growth<5)%>%
  group_by(geography,yyyyq)%>%
  filter(n()>=10)%>% # Removing the time*state observation if less than 10 observations
  ungroup()

time<-unique(stateData$yyyyq)
state<-unique(stateData$geography)

dwrState<-matrix(nrow=length(time),ncol=length(state),dimnames = list(time,state))

for (state_var in 1:length(state)){ #length(state)
  for (time_var in 1:length(time)){ #length(time)
    dat<-as.matrix(stateData%>%filter(geography==state[state_var],yyyyq==time[time_var])%>%select(wage_growth))
    if(length(dat)>0){
      distr<-ecdf(dat)
      dwrState[time_var,state_var]<-1-distr(2*median(dat))-distr(0)
    }
  }
}



################################################################################v
# For dwsNaics:
naicsData<-THEdata%>%
  unite(id,sex,agegrp,firmsize,geography)%>%
  arrange(industry,id,yyyyq)%>%
  mutate(
    wage_growth=ifelse(id==lag(id),EarnS/lag(EarnS)-1,NA)
  )%>%
  filter(!is.na(wage_growth),wage_growth>-0.5,wage_growth<5)%>%
  group_by(industry,yyyyq)%>%
  filter(n()>=10)%>% # Removing the time*naics observation if less than 10 observations
  ungroup()

time<-unique(naicsData$yyyyq)
naics<-unique(naicsData$industry)

dwrNaics<-matrix(nrow=length(time),ncol=length(naics),dimnames = list(time,naics))

for (naics_var in 1:length(naics)){ #length(naics)
  for (time_var in 1:length(time)){ #length(time)
    dat<-as.matrix(naicsData%>%filter(industry==naics[naics_var],yyyyq==time[time_var])%>%select(wage_growth))
    if(length(dat)>0){
      distr<-ecdf(dat)
      dwrNaics[time_var,naics_var]<-1-distr(2*median(dat))-distr(0)
    }
  }
}


dwrFirmsize<-as_tibble(as.data.frame(dwrFirmsize))%>%
  rownames_to_column()%>%
  gather(key=firmsize,value=dwr,2:6)%>%
  rename(yyyyq=rowname)

dwrState<- as_tibble(as.data.frame(dwrState))%>%
  rownames_to_column()%>%
  gather(key=state,value=dwr,2:52)%>%
  rename(yyyyq=rowname)

dwrNaics<- as_tibble(as.data.frame(dwrState))%>%
  rownames_to_column()%>%
  gather(key=naics,value=dwr,2:308)%>%
  rename(yyyyq=rowname)

dwrFirmsize%>%filter(!is.na(dwr))%>%write_csv("data/dwrFirmsize1.csv")
dwrState%>%filter(!is.na(dwr))%>%write_csv("data/dwrState1.csv")
dwrNaics%>%filter(!is.na(dwr))%>%write_csv("data/dwrNaics1.csv")

# Old way:
# write_csv(as_tibble(as.data.frame(dwrFirmsize))%>%rownames_to_column(),"data/dwrFirmsize.csv")
# write_csv(as_tibble(as.data.frame(dwrState))%>%rownames_to_column(),"data/dwrState.csv")
# write_csv(as_tibble(as.data.frame(dwrNaics))%>%rownames_to_column(),"data/dwrNaics.csv")

# Load them:
# dwrFirmsize<-read_csv("data/dwrFirmsize1.csv")#%>%gather(key=firmsize,value=dwr,2:6)%>%rename(yyyyq=rowname)
# dwrState<-read_csv("data/dwrState1.csv")#%>%gather(key=state,value=dwr,2:52)%>%rename(yyyyq=rowname)
# dwrNaics<-read_csv("data/dwrNaics1.csv")#%>%gather(key=naics,value=dwr,2:308)%>%rename(yyyyq=rowname)
