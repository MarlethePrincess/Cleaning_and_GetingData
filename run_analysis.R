# Fuente -> https://class.coursera.org/getdata-030/human_grading/view/courses/975114/assessments/3/submissions
# course project Getting and Cleaning Data

install.packages("data.table")
#-----------------------------
# Cargando las Librerias
#------------------------------
library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

# Descargando el archivo y descomprimiendo

#-------------------------------
# temp <- tempfile()
# download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "C:\\Users\\ma.cabral\\Videos\\Especialidad Analisis Datos\\Getting and Cleaning Data\\Dataset.zip"
download.file(fileUrl, destfile)
unzip("Dataset.zip")
setwd("C:\\Users\\ma.cabral\\Videos\\Especialidad Analisis Datos\\Getting and Cleaning Data\\UCI HAR Dataset\\")
# -------------------------------------
# Descomprimiendo el archivo
#--------------------------------------
# unzip(datatemp.csv, list = TRUE) #This provides the list of variables and I choose the ones that are applicable for this data set
YTest <- read.table("test\\y_test.txt", header = FALSE, sep = "")
XTest <- read.table("test\\X_test.txt", header = FALSE, sep = "")
SubjectTest <- read.table("test/subject_test.txt", header = FALSE, sep = "")
YTrain <- read.table("train/y_train.txt", header = FALSE, sep = "")
XTrain <- read.table("train/X_train.txt", header = FALSE, sep = "")
SubjectTrain <- read.table("train/subject_train.txt", header = FALSE, sep = "")
Features <- read.table("features.txt", header = FALSE, sep = "")
#unlink(datatemp.csv) # very important to remove this
#--------------------------------------------
# Cleaning de Data
#--------------------------------------------
colnames(XTrain) <- t(Features[2]) # obteniendo la traspuesta de este vector
colnames(XTest) <- t(Features[2])
#------------------------------------------------
# chequeadndo no IDs comunes

XTrain$activities <- YTrain[, 1]
XTrain$Participantes <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$Participantes <- SubjectTest[, 1]

#----------------------------------------------
# Course Project Part 1
#----------------------------------------------

MData <- rbind(XTrain, XTest)
duplicated(colnames(MData))
MData <- MData[, !duplicated(colnames(MData))]

#----------------------------------------------
# Course Project Part 2
#----------------------------------------------
# Extracts only the measurements on the mean and standard deviation for each measurement.

Mean <- grep("mean()", names(MData), value = FALSE, fixed = TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
MiMeanMatrix <- MData[Mean]
# For STD
STD <- grep("std()", names(MData), value = FALSE)
MiSTDMatrix <- MData[STD]

#----------------------------------------------
# Course Project Part 3
#----------------------------------------------

# Uses descriptive activity names to name the activities in the data set

MData$activities <- as.character(MData$activities)
MData$activities[MData$activities == 1] <- "Caminar"             
MData$activities[MData$activities == 2] <- "Caminar hacia Arriba"   
MData$activities[MData$activities == 3] <- "Caminar hacia Abajo"  
MData$activities[MData$activities == 4] <- "Sentado" 
MData$activities[MData$activities == 5] <- "De Pie" 
MData$activities[MData$activities == 6] <- "Acostado"
MData$activities <- as.factor(MData$activities)

#--------------------------------------------------
# Course Project Part 4
#----------------------------------------------
names(MData)  # survey the data

names(MData) <- gsub("Acc", "Acelerador", names(MData))
names(MData) <- gsub("Mag", "Magnitude", names(MData))
names(MData) <- gsub("Gyro", "Gyroscopio", names(MData))
names(MData) <- gsub("^t", "timpo", names(MData))
names(MData) <- gsub("^f", "frequencia", names(MData))

#----------------------------------------------
MData$Participantes <- as.character(MData$Participantes)
MData$Participantes[MData$Participantes == 1] <- "Participante 1"
MData$Participantes[MData$Participantes == 2] <- "Participante 2"
MData$Participantes[MData$Participantes == 3] <- "Participante 3"
MData$Participantes[MData$Participantes == 4] <- "Participante 4"
MData$Participantes[MData$Participantes == 5] <- "Participante 5"
MData$Participantes[MData$Participantes == 6] <- "Participante 6"
MData$Participantes[MData$Participantes == 7] <- "Participante 7"
MData$Participantes[MData$Participantes == 8] <- "Participante 8"
MData$Participantes[MData$Participantes == 9] <- "Participante 9"
MData$Participantes[MData$Participantes == 10] <- "Participante 10"
MData$Participantes[MData$Participantes == 11] <- "Participante 11"
MData$Participantes[MData$Participantes == 12] <- "Participante 12"
MData$Participantes[MData$Participantes == 13] <- "Participante 13"
MData$Participantes[MData$Participantes == 14] <- "Participante 14"
MData$Participantes[MData$Participantes == 15] <- "Participante 15"
MData$Participantes[MData$Participantes == 16] <- "Participante 16"
MData$Participantes[MData$Participantes == 17] <- "Participante 17"
MData$Participantes[MData$Participantes == 18] <- "Participante 18"
MData$Participantes[MData$Participantes == 19] <- "Participante 19"
MData$Participantes[MData$Participantes == 20] <- "Participante 20"
MData$Participantes[MData$Participantes == 21] <- "Participante 21"
MData$Participantes[MData$Participantes == 22] <- "Participante 22"
MData$Participantes[MData$Participantes == 23] <- "Participante 23"
MData$Participantes[MData$Participantes == 24] <- "Participante 24"
MData$Participantes[MData$Participantes == 25] <- "Participante 25"
MData$Participantes[MData$Participantes == 26] <- "Participante 26"
MData$Participantes[MData$Participantes == 27] <- "Participante 27"
MData$Participantes[MData$Participantes == 28] <- "Participante 28"
MData$Participantes[MData$Participantes == 29] <- "Participante 29"
MData$Participantes[MData$Participantes == 30] <- "Participante 30"
MData$Participantes <- as.factor(MData$Participantes)
#----------------------------------------------
# Course Project Part 5
#----------------------------------------------

MData.dt <- data.table(MData)
#This takes the mean of every column broken down by Participantes and activities
TidyData <- MData.dt[, lapply(.SD, mean), by = 'Participantes,actividades']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)

#----------------------------------------------
# Extra Credit
#----------------------------------------------
library(knitr)
knit2html("codebook.Rmd");
