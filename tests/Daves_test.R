library(dplyr)
library(tidyverse)
library (haven)
library(SASxport)
rm(list = ls())
# list.files("R", pattern = "\\.R$", full.names = TRUE) %>% walk(source)
sas_df <- readRDS("./tests/sas_df.rds")
dd <-readRDS("./dd.rds")

cc = list()
#cc[['DF1']] <- sas_df
#cc[['DF2']] <- sas_df
cc[['DD1']] <- dd$SAS.df
cc[['DD2']] <-dd$SAS.df
#dd <-read_xpt('tto.xpt')
#list.files("R", pattern = "\\.R$", full.names = TRUE) %>% walk(source)
aa<- readRDS('~/Desktop/DCH/data-transfers/DTS from GIT/data-transfers/training/SAS_list.RDS')
write.xport(aa,
            file="tt.xpt",
            cDate=as.POSIXlt(Sys.time()),
            osType="LIN X64", sasVer="9.1", autogen.formats=FALSE, verbose=F)

write.xport(cc,
            file="tt.xpt",
            cDate=strptime("23FEB18:05:53:58", format="%d%b%y:%H:%M:%S"),
            osType="LIN X64", sasVer="9.1", autogen.formats=FALSE, verbose=F)

'HEADER RECORD*******OBSV8   HEADER RECORD!!!!!!!              1                 '   

'HEADER RECORD*******OBSV8   HEADER RECORD!!!!!!!              5                 '

'HEADER RECORD*******OBSV8   HEADER RECORD!!!!!!!              1                 '

'HEADER RECORD*******OBSV8   HEADER RECORD!!!!!!!             12                 '

'HEADER RECORD*******LABELV8 HEADER RECORD!!!!!!!3                               '

'HEADER RECORD*******LABELV8 HEADER RECORD!!!!!!!3                               '
'HEADER RECORD*******LABELV8 HEADER RECORD!!!!!!!3                               '