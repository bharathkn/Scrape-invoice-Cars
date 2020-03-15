setwd("X:/Workspace")
library(tabulizer)
library(pdftools)
library(tidyverse)
library(stringr)
#modelno <- pdflist[[1]]%>%
  #str_squish()%>%
  #grep("Make")

files <- list.files(pattern = "pdf$")
files
pdflist <- lapply(files, pdf_text)
str(pdflist)
length(pdflist)

#Building a function to pull out the Preliminary details from the invoices

BasicDetails <- function(){
#defining place holders for vehicle Registration, Make and Model
VehicleNo <- c()
VehicleMake <- c()
VehicleModel <- c()

#initiating a basic for loop to detect words Registration, Make and Model and provide
#us with output corresponding to the values
for (counter in 1:length(pdflist)){
  j<- unlist(str_split(str_squish(pdflist[[counter]][1])," "))
  VehicleNo[counter] <- j[14]
  VehicleMake[counter] <- j[grep("Make",j)+1]
  VehicleModel[counter] <- paste0(j[grep("Model",j)+1],sep=" ", j[grep("Model",j)+2])
  #print(paste0("The Vehicle Registration number of the bill no ", counter," is ",VehicleNo))
  #print(paste0("The make of the vehicle is ",VehicleMake))
  #print(paste0("and the model no is ", VehicleModel))
}
PrelimInfo <- data.frame(VehicleNo,VehicleMake,VehicleModel)#creating a dataframe with all the information
View(PrelimInfo)#Display the data frame which we created in the previous step
}#End of function Basic Details

#----------------------------------------------

#extracting Repair information from table we have


RRDetails <- function(){
for(listno in 1: length(pdflist)){

  
TabExtracter <- extract_tables(files[listno], encoding = "UTF-8")
#str(TabExtracter)


firstpage <- as.data.frame(unlist(TabExtracter[[1]]))
firstpage$V1 <- as.character(firstpage$V1)
firstpage$V2 <- as.character(firstpage$V2)
#View(firstpage$V1)


(secondpage <- as.data.frame(unlist(TabExtracter[[2]])))
secondpage$V2 <- as.character(secondpage$V2)
secondpage$V3 <- as.character(secondpage$V3)
#View(secondpage$V2)


pdfnutshell <- rbind(firstpage[,1:3], secondpage[,1:3])
#View(pdfnutshell)


(RepaCor <- grep("^REPAIR", pdfnutshell$V1))
RepaPart[listno] <- pdfnutshell$V1[RepaCor]
#RepaPart



ReplCor<- which(str_length(pdfnutshell$V2)>11)
if(is_empty(ReplCor)){
  ReplCor <- which(str_length(pdfnutshell$V3)>11)
} else if(is_empty(ReplCor)) {
    ReplCor <- which(str_length(pdfnutshell$V1) > 11)
  }

ReplPart[listno]<- pdfnutshell$V1[ReplCor]
#ReplPart



}#END of FOR LOOP

} #END OF FUNCTION


#path='X:/Workspace/KE19OXL.pdf'
#lst <- extract_tables(path, encoding="UTF-8")



#str(lst)
#h <- as.data.frame(lst[[1]][,1:ncol(lst[[1]])])
#i <- as.data.frame(lst[[1]][,1:ncol(lst[[2]])])
#j <- as.data.frame(lst[[1]][,1:ncol(lst[[3]])])
#str(h)

#View(h)
#convert it to data frame for data extraction
#firstpage <- as.data.frame(firstpage)

#RepairCordinates <- grep("^REPAIR", firstpage$V1)
#(RepairParts <-  firstpage$V1[RepairCordinates])



#-----------------------------------------------

#extracting Replace information
#secondpage <- as.data.frame(secondpage)

#str(secondpage)
#View(secondpage)
#ReplaceCordinates <- which(str_length(secondpage$V2)>11)
#ReplaceParts <- secondpage$V1[ReplaceCordinates]
#ReplaceParts
#TabExtracter <- extract_tables(files[4], encoding = "UTF-8")
#str(TabExtracter)
#firstpage <- as.data.frame(unlist(TabExtracter[[1]]))
#firstpage$V1 <- as.character(firstpage$V1)
#View(firstpage$V1)
#(secondpage <- as.data.frame(unlist(TabExtracter[[2]])))
#secondpage$V2 <- as.character(secondpage$V2)
#View(secondpage$V2)
#pdfnutshell <- rbind(firstpage[,1:2], secondpage[,1:2])
#View(pdfnutshell)
#(RepaCor <- grep("^REPAIR", pdfnutshell$V1))
#RepaPart <- pdfnutshell$V1[RepaCor]
#RepaPart
#(ReplCor <- which(str_length(pdfnutshell$V2)>11))
#ReplPart <- pdfnutshell$V1[ReplCor]
#ReplPart
#}


#-----------------------------------------------


#condensing the above code to suit our situation
