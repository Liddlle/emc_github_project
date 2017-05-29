library(jsonlite)
library(rvest)
library(pbapply)
library(data.table)
options(digits = 4)

#https://www.rdocumentation.org/trends?page1=3&sort1=direct&page2=1&sort2=total&page3=1&page4=1


all.urls<-paste0('https://www.rdocumentation.org/trends?page1=', 101:1290, '&sort1=direct&page2=1&sort2=total&page3=1&page4=1')
#all.urls<-paste0('https://www.rdocumentation.org/trends?page1=', 1:1290, '&sort1=direct&page2=1&sort2=total&page3=1&page4=1')

main<-function(url.x){
  x<-read_html(url.x)
  x<-html_table(x)
  x<-data.table(x[[1]])
  return(x)
  Sys.sleep(5)
}

all.h1b<-pblapply(all.urls, main)
all.h1b<-rbindlist(all.h1b)


#all.h1b3 = rbind(all.h1b3, all.h1b)
#all.h1b3 = all.h1b3 %>% unique()

readr::write_csv(all.h1b3, "all_packages.csv")
