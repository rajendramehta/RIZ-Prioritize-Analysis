# RIZ-Prioritize-Analysis
#RIZ are the files which are base file in predictive model and having unstructured data and which need to be formatted and also replace missing values by mean and detect outlier and male a separated file for that.


#/********************************************************************************/#
#/*     Objective : R Code for RI Preparation request                                      #                           */                           */
#/*                                                                              */#
#/*     Prepared By : Rajendra Mehta                                               #
#*/     date        :27/02/17                                                    */#  
#/********************************************************************************/#
rm(list=(ls()))

library(RGtk2)
options(guiToolkit="RGtk2")
library(gWidgetsRGtk2)

w <- gwindow("ORAL PURCHASE",visible=T)
g <- ggroup(horizontal = F, container = w)

ga <- gframe("SELECT FILE/FOLDER PATH",pos=0.5,horizontal = F, container = g)
gb <- gframe("Enter PNO",pos=0.5,horizontal = T, container = g)
gi <- gframe(horizontal = F,pos=0.5, container = g)

gj <- gframe(horizontal = F,pos=0.5, container = g)

browse.file1 <- gfilebrowse("SELECT CMBDB DBF FILE",container = ga,quote=F)
browse.file2 <- gfilebrowse("SELECT OUTLET CSV FILE",container = ga,quote=F)
browse.file3 <- gfilebrowse("SELECT RGN_TYP CSV FILE",container = ga,quote=F)
browse.file5 <- gfilebrowse("SELECT OUTPUT FOLDER PATH",container = ga,quote=F,type="selectdir")


b1<-glabel("INSERT_PNO (e.g.1 for 381):",cont=gb)
b11<-gedit("NULL",cont=gb)



button1 <- gbutton(text="OK- TO SUBMIT INPUT", container = gi,
                   handler = function(R, ...) {
                     CMBDB<<- svalue(browse.file1)
                     OUTLET<<- svalue(browse.file2)
                     RGN_TYP<<- svalue(browse.file3)
                     OUTPUT_File<<- svalue(browse.file5)
                     PNO<<-svalue(b11)
                     
                     })

###################################################################################
RIZ<-function(R,...){
  
library(ff)
library(ffbase)
library(foreign)
library(plyr)
library(data.table)
library(sqldf)
#Reading CMBDB and RGN_TYP file from path
CMBDB_1<-read.dbf(file=CMBDB)
RGN_TYP_1<-read.csv(file=RGN_TYP) 
RGN_TYP_2<-unique(RGN_TYP_1)
OUTLET_1<-read.csv(file=OUTLET)
x<-read.csv("Sales_New.txt")
CMBDB_2<-CMBDB_1[c("OUTLET","PNO","RGN","TYP","TYB","VISDATE")]
CMBDB_3<-CMBDB_2[which(CMBDB_2$PNO == (PNO-1)),]

Final_File<-merge(x=OUTLET_1,y=CMBDB_3,by.x=c("OUTLET"),by.y=c("OUTLET"),all.x = TRUE,trace=T)
Final_File_NA<-Final_File[which(is.na(Final_File$PNO)),]
Final_File_1<-Final_File[which(!is.na(Final_File$PNO)),]

Final_File_1$key<-paste(Final_File_1$RGN, Final_File_1$TYP, sep="_")
Final_File_1_1<-Final_File_1[c("key")]

Final_File_2<-merge(x=Final_File_1_1,y=RGN_TYP_2,by.x=c("key"),by.y=c("key"),all.x = TRUE,trace=T)
Final_File_3<-merge(x=Final_File_1,y=Final_File_2,by.x=c("key"),by.y=c("key"),all.x = TRUE,trace=T)
Final_File_4<-unique(Final_File_3)
Final_File_5<-sqldf("SELECT CELL,COUNT(OUTLET) AS OUTLET_SUM FROM Final_File_4 GROUP BY OUTLET")

write.csv(Final_File_4,file=paste0(OUTPUT_File,"\\","RIZ_OUTPUT",".csv"),row.names=FALSE)
write.csv(Final_File_5,file=paste0(OUTPUT_File,"\\","PIVOT_TABLE",".csv"),row.names=FALSE)
write.csv(Final_File_NA,file=paste0(OUTPUT_File,"\\","RIZ_OUTPUT_NA",".csv"),row.names=FALSE)
dispose(w)}
button10<-gbutton("RUN",cont=gj,handler=RIZ)
