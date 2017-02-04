library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(tibble)

path = "/media/espen/My Passport/QCEW/SIC/"
  #"/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities/data/wages_QCEW/SIC/"

map_names<-dir(path)

out_file<-list()
index=0

#
for(j in 1:2){ #length(map_names)
  file_names <- dir(paste(path,map_names[j],sep = ""), pattern =".csv")
  for(i in 1:length(file_names)){
    index=index+1
    #file <- read_csv(paste(path,file.names[i],sep=""))#,header=TRUE, sep=";", stringsAsFactors=FALSE)
    out_file[[index]] <-read_csv(paste(path,map_names[j],"/",file_names[i],sep=""),
                                 col_types=cols(
                                   area_fips=col_character(),
                                   own_code=col_integer(),
                                   industry_code=col_character(),
                                   agglvl_code=col_integer(),
                                   size_code=col_integer(),
                                   year=col_integer(),
                                   qtr=col_character(),
                                   disclosure_code=col_character(),
                                   area_title=col_character(),
                                   own_title=col_character(),
                                   industry_title=col_character(),
                                   agglvl_title=col_character(),
                                   size_title=col_character(),
                                   annual_avg_estabs_count=col_integer(),
                                   annual_avg_emplvl=col_integer(),
                                   total_annual_wages=col_integer(),
                                   taxable_annual_wages=col_integer(),
                                   annual_contributions=col_integer(),
                                   annual_avg_wkly_wage=col_integer(),
                                   avg_annual_pay=col_integer()
                                   )
                                 )%>%
      filter(agglvl_title=="State, 4-digit SIC -- by ownership sector",disclosure_code!="N",own_title=="Private")%>%
      dplyr::select(area_fips,industry_code,size_code,year,qtr,area_title,industry_title,annual_avg_estabs_count,annual_avg_emplvl,
                    total_annual_wages,taxable_annual_wages,annual_contributions,annual_avg_wkly_wage,avg_annual_pay)
  }
  print(j)
}


out_file2<-dplyr::bind_rows(out_file)

write_csv(out_file2,"/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities/data/QCEW_SIC_1975-2000.csv")


