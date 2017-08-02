#load libraries for for melt and column reorder
library(reshape2)
library(data.table)
library(plyr)
library(readxl)
#import look up lists
analyteLookup <- read_excel("crosswalks/xwalk DPR-SURF_AnalyteNames_v2.xlsx", col_names=TRUE)
projectLookup <- read_excel("crosswalks/DPR-SURF_ProjectXwalks.xlsx", col_names=TRUE)
parentProjectLookup <- read_excel("crosswalks/DPR-SURF_Program_ParentProject.xlsx", col_names=TRUE)
labLookup <- read_excel("crosswalks/DPR-SURF_LabNames XWalk.xlsx", col_names=TRUE)
agencyLookup <- read_excel("crosswalks/DPR-SURF_AgencyNames_XWalk.xlsx", col_names=TRUE)
collectionMethodLookup <- read_excel("crosswalks/DPR-SURF_CollectionMethodsXWalk.xlsx", col_names=TRUE)
sampleTypeLookup <- read_excel("crosswalks/SURF_Sample_Types_jr.xlsx", col_names=TRUE)
countyLookup <- read_excel("crosswalks/SURF_countyName_XWalk.xlsx", col_names=TRUE)
methodLookup <- read_excel("crosswalks/Copy of xwalk DPR-SURF_MethodNames_v2.xlsx", col_names=TRUE)
collectionDeviceLookup <- read_excel("crosswalks/CollectionDevice_XWALK.xlsx", col_names=TRUE)
#import water data
waterData <- read.csv(file="water_forSFEI.csv", header=TRUE, sep=",")
#import sediment data
sedimentData <- read.csv(file="sediment_forSFEI.csv", header=TRUE, sep=",")
#add matrixcode field
waterData$MatrixCode<- "samplewater"
sedimentData$MatrixCode<- "sediment"
#melt TOC and moisture data to new data frame (ignoring Result, MDL,RL, and analyte fields for analyses they were measured with)
sedimentTOCMoisture <- melt(sedimentData, id.vars=c("STUDY_CD","COUNTY_CD","LOC_CD","SAMP_DATE","EXTRAC_DATE","ANLY_DATE","AGENCY_CD","COLL_METH_CD","ANLY_METH_CD","SAMPLER_CD","SAMP_TYPE_CD","STORM_FLAG","LAB_CD","RMK_FLAG","REMARKS","SAMP_TIME","TRACE_FLAG","SAMP_ID2","Study_description","Weblink","RECORD_ID","Source","MatrixCode"), measure.vars=c("TOC_PERC","MOISTURE_PERC"),variable.name = "CHEM_CD", value.name = "CONC")
#remove TOC and MOISTURE columns from Sediment data
sedimentData$TOC_PERC <-NULL
sedimentData$MOISTURE_PERC <-NULL
#add RL and MDL fields to TOC and MOISTURE data with defaults
#sedimentTOCMoisture$RL <- -88
sedimentTOCMoisture$LOQ<- -88
sedimentTOCMoisture$MDL <- -88
#Change TOC and MOISTURE values to lookup list number values (from AnalyteName crosswalk)
sedimentTOCMoisture$CHEM_CD <- as.character(sedimentTOCMoisture$CHEM_CD)
sedimentTOCMoisture$CHEM_CD[sedimentTOCMoisture$CHEM_CD == "TOC_PERC"] <- 999998
#sedimentTOCMoisture<- within(sedimentTOCMoisture, CHEM_CD[CHEM_CD == "TOC_PERC"] <- 999998
sedimentTOCMoisture$CHEM_CD[sedimentTOCMoisture$CHEM_CD == "MOISTURE_PERC"] <- 999999
#sedimentTOCMoisture<- within(sedimentTOCMoisture, CHEM_CD[CHEM_CD == "MOISTURE_PERC"] <- 999999
sedimentTOCMoisture$CHEM_CD <- as.integer(sedimentTOCMoisture$CHEM_CD)
#reorder columnames to match
#colOrder <- colnames(sedimentData)
#setcolorder(sedimentTOCMoisture,colOrder)
#combine water, sediment, TOC and Moisture results to one new dataframe
combinedData <- rbind(waterData, sedimentData, sedimentTOCMoisture)
#join the lookup lists
combinedData <- join(combinedData, analyteLookup, by = "CHEM_CD", type = "left", match = "all")
combinedData <- join(combinedData, projectLookup, by = "STUDY_CD", type = "left", match = "all")
combinedData <- join(combinedData, labLookup, by = "LAB_CD", type = "left", match = "all")
combinedData <- join(combinedData, agencyLookup, by = "AGENCY_CD", type = "left", match = "all")
combinedData <- join(combinedData, collectionMethodLookup, by = "COLL_METH_CD", type = "left", match = "all")
combinedData <- join(combinedData, methodLookup, by = "ANLY_METH_CD", type = "left", match = "all")
combinedData <- join(combinedData, countyLookup, by = "COUNTY_CD", type = "left", match = "all")
combinedData <- join(combinedData, sampleTypeLookup, by = "SAMP_TYPE_CD", type = "left", match = "all")
combinedData <- join(combinedData, collectionDeviceLookup, by = "SAMPLER_CD", type = "left", match = "all")
#Remove unneeded columns
dropColumns <- c("NAME","Comments","CAS Numbers","STUDY_DESC","CEDEN ProjectDescr","COLL_METH_DESC","CEDEN Description","Comments CollectionMethod","ANLY_METH_DESC","MethodComments","SAMP_TYPE_DESC","CEDEN Agency Name","AGENCY_DESC","LAB_DESC","Comments LabAgency")
combinedData <-combinedData[ , !(names(combinedData) %in% dropColumns)]
#Remove blank headed columns renamed as "X_..."
cols<- colnames(combinedData)
blankCols <- cols[like(cols,"X_")] 
combinedData <-combinedData[ , !(names(combinedData) %in% blankCols)]
#change field names to CEDEN names
colnames(combinedData)[which(colnames(combinedData) %in% c("STUDY_CD","COUNTY_CD","LOC_CD","SAMP_DATE","EXTRAC_DATE","ANLY_DATE","AGENCY_CD","CHEM_CD","CONC","LOQ","COLL_METH_CD","ANLY_METH_CD","SAMPLER_CD","SAMP_TYPE_CD","STORM_FLAG","LAB_CD","RMK_FLAG","REMARKS","SAMP_TIME","TRACE_FLAG","SAMP_ID2","MDL","RECORD_ID","Study_description","Weblink","CEDEN AnalyteName","CEDEN ProjectCode","CEDEN ProjectName","CEDEN LabAgencyCode","CEDEN AgencyCode","CEDEN CollectionMethod","CEDEN Method Name","SampleType","CEDEN CollectionDeviceName","CollectionDevice Comments/Questions") )] <- c("ProjectCode_SURF","CountyCode_SURF","StationNumber_SURF","SampleDate","DigestExtractDate","AnalysisDate","AgencyCode_SURF","AnalyteCode_SURF","Result","RL","CollectionMethodCode_SURF","AnalyticalMethodCode_SURF","CollectionDeviceCode_SURF","SampleTypeCode_SURF","SampleComments","AnalyticalAgencyCode_SURF","CompCode","LabComments","SampleTime","ResQualCode","SampleID","MDL","Record_ID_SURF","ProjectDescription","ProjectURL","AnalyteName","ProjectCode","ProjectName","LabAgencyCode","AgencyCode","CollectionMethodCode","MethodName","SampleTypeCode","CollectionDeviceName","CollectionDeviceComments")
#Add units as ug/L
combinedData$UnitName <- "ug/L"
#Concat the county and LOC_CD as StationCode 
combinedData$StationCode <- paste(combinedData$CountyName, combinedData$StationNumber_SURF, sep=" ")
#Concat the ProjectCode and LabAgencyCode as LabBatch 
combinedData$LabBatch <- paste(combinedData$ProjectCode, combinedData$LabAgencyCode,sep="_")
#format sample time as time
combinedData$SampleTime <- substr(as.POSIXct(sprintf("%04.0f", combinedData$SampleTime), format='%H%M'), 12, 16)
#Replace empty values with defaults
combinedData$SampleTime[is.na(combinedData$SampleTime)] <- '00:00'
#combinedData$CompCode[is.na(combinedData$CompCode)] <- "NR"
combinedData$MDL[is.na(combinedData$MDL)] <- -88
combinedData$RL[is.na(combinedData$RL)] <- -88
#combinedData$CollectionDeviceName[is.na(combinedData$CollectionDeviceName)] <- paste(combinedData$SAMPLER_DESC, combinedData$CollectionDeviceComments, sep="-")
combinedData<- within(combinedData, CollectionDeviceName[is.na(combinedData$CollectionDeviceName)] <- paste(combinedData$SAMPLER_DESC[is.na(combinedData$CollectionDeviceName)], combinedData$CollectionDeviceComments[is.na(combinedData$CollectionDeviceName)], sep="-- ")
)

#Add not included required fields with defaults
combinedData$EventCode <- "WQ"
combinedData$LocationCode <- "Not Recorded"
combinedData$CollectionDepth <- -88
combinedData$UnitCollectionDepth <-"m"
combinedData$Replicate <- 1
combinedData$GeometryShape <- "Point"
combinedData$LabReplicate <- 1
combinedData$DilutionFactor <- 1
combinedData$PrepPreservationName <- "Not Recorded"
combinedData$PrepPreservationDate <- "1/1/1950"

#update sampling info from SAMP_TYPE_CD indicated in fields other than SampleTypeCode
combinedData<- within(combinedData, MatrixCode[SampleTypeCode_SURF %in% c(9,10)] <- 'runoff')
#assuming duplicates are sample replicates and not field blinf dupes
combinedData<- within(combinedData, Replicate[SampleTypeCode_SURF == 7] <- 2)
combinedData<- within(combinedData, SampleTypeCode[SampleTypeCode_SURF == 7] <- "Grab")

#update collectionMethodCodes dependent on matrix
combinedData<- within(combinedData, CollectionMethodCode[CollectionMethodCode == 'Water_Grab or Sed_Grab' & MatrixCode == 'sediment'] <- 'Sed_Grab')
combinedData<- within(combinedData, CollectionMethodCode[CollectionMethodCode == 'Water_Grab or Sed_Grab' & MatrixCode == 'samplewater'] <- 'Water_Grab')

#Remove SURF Code fields
dropColumns <- c("CountyCode_SURF","StationNumber_SURF","CollectionMethodCode_SURF","AnalyticalMethodCode_SURF","CollectionDeviceCode_SURF","SampleTypeCode_SURF","AnalyticalAgencyCode_SURF","AgencyCode_SURF","AnalyteCode_SURF","CEDEN LabAgencyName","SAMPLER_DESC","CollectionDeviceComments","ProjectCode_SURF")
combinedData <-combinedData[ , !(names(combinedData) %in% dropColumns)]
#export the file 
write.csv(combinedData, file = paste("SURFtoCEDEN", Sys.Date(),".csv", sep="_"))
