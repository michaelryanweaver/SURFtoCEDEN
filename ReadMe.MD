#Read Me
##SURF to CEDEN Data format Transformer


##Set up to run file
1. the SURF_data_SCRUB.R file should be in a folder that is set as your R working directory
2. There should be a "crosswalks" subdirectory in this folder that contains the following files:
	1. CollectionDevice_XWALK.xlsx
	2. Copy of xwalk DPR-SURF_MethodNames_v2.xlsx
	3. DPR-SURF_AgencyNames_XWalk.xlsx
	4. DPR-SURF_CollectionMethodsXWalk.xlsx
	5. DPR-SURF_LabNames XWalk.xlsx
	6. DPR-SURF_ProjectXwalks.xlsx
	7. SURF_MethodNames_NotCEDEN.xlsx
	8. SURF_Sample_Types_jr.xlsx
	9. SURF_countyName_XWalk.xlsx
	10. xwalk DPR-SURF_AnalyteNames_v2.xlsx
3. The script expects data files in the main folder named "water_forSFEI.csv" for water data and "sediment_forSFEI.csv" for sediment data.  If you want your files named something else, replace the names in the script on the following lines:
	waterData <- read.csv(file="water_forSFEI.csv", header=TRUE, sep=",")
	sedimentData <- read.csv(file="sediment_forSFEI.csv", header=TRUE, sep=",")
4. The following R packages should be installed:
* reshape2
* data.table
* plyr
* readxl
if they're not installed run install.packages("package-name-here") in the R console prior to 

##Output
When you run the file in the R console with the correct set up, the script will output a file named SURFtoCEDEN_(current date).csv

