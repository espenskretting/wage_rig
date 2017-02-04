library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(tibble)

path = "/media/espen/My Passport/QCEW/naics/files/"
  #"/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities/data/wages_QCEW/SIC/"

map_names<-dir(path)

out_file<-list()
index=0

#
for(j in 1:length(map_names)){ #
  file_names <- dir(paste(path,map_names[j],sep = ""), pattern =".csv")
  for(i in 1:length(file_names)){
    index=index+1
    #file <- read_csv(paste(path,file.names[i],sep=""))#,header=TRUE, sep=";", stringsAsFactors=FALSE)
    out_file[[index]] <-read_csv(paste(path,map_names[j],"/",file_names[i],sep=""))
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
                                   qtrly_estabs_count=col_integer(),
                                   month1_emplvl=col_integer(),
                                   month2_emplvl=col_integer(),
                                   month3_emplvl=col_integer(),
                                   total_qtrly_wages=col_integer(),
                                   taxable_qtrly_wages=col_integer(),
                                   qtrly_contributions=col_integer(),
                                   avg_wkly_wage=col_integer(),
                                   oty_disclosure_code=col_integer(),
                                   oty_qtrly_estabs_count_chg=col_integer(),
                                   oty_qtrly_estabs_count_pct_chg=col_integer(),
                                   oty_month1_emplvl_chg=col_integer(),
                                   oty_month1_emplvl_pct=col_integer(),
                                   oty_month2_emplvl_chg=col_integer(),
                                   oty_month2_emplvl_pct=col_integer(),
                                   oty_month3_emplvl_chg=col_integer(),
                                   oty_month3_emplvl_pct=col_integer(),
                                   oty_total_qtrly_wages_chg=col_integer(),
                                   oty_total_qtrly_wages_pct=col_integer(),
                                   oty_taxable_qtrly_wages_chg=col_integer(),
                                   oty_qtrly_contributions_chg=col_integer(),
                                   oty_qtrly_contributions_pct=col_integer(),
                                   oty_avg_wkly_wage_chg=col_integer(),
                                   oty_avg_wkly_wage_pct=col_integer()
                                   )
                                 )%>%filter(disclosure_code!="N",own_title=="Private")%>%#agglvl_title=="State, 4-digit SIC -- by ownership sector",
      dplyr::select(area_fips,industry_code,size_code,year,qtr,area_title,industry_title,avg_wkly_wage,qtrly_estabs_count,total_qtrly_wages,
                    oty_qtrly_estabs_pct_chg,oty_total_qtrly_wages_pct_chg,oty_taxable_qtrly_wages_pct_chg,oty_qtrly_contributions_pct_chg,oty_avg_wkly_wage_pct_chg,
                    oty_month1_emplvl_pct_chg,oty_month2_emplvl_pct_chg,oty_month3_emplvl_pct_chg
                    )
  }
  print(j)
}


out_file2<-dplyr::bind_rows(out_file)

write_csv(out_file2,"/home/espen/espenskretting@gmail.com/ABProsjekter/Asset prices and nomical rigidities/data/QCEW_NAICS_1990-2016.csv")


