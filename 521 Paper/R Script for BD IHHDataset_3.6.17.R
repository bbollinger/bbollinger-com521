IHHDataset <- read.csv("~/Desktop/Winter 2017/COM 521/Paper/Working With/BDPR70FL.csv", header=TRUE)
View(IHHDataset)

####BELOW ARE HOUSEHOLD LEVEL VARIABLES
HHMembersTotal <- (IHHDataset$HV009) #total household members in numberic form
TwomeninHouse <- (IHHDataset$HV010) #total women in house

TypeofResidence <- (IHHDataset$HV025) #type of place of residence
TypeofResidence[TypeofResidence==1] <- "Urban"
TypeofResidence[TypeofResidence==2] <- "Rural"

AgeHH <- IHHDataset$HV220 #age of head of house

SexHH <- (IHHDataset$HV219) #sex of head of household (1=male, 2=female)
SexHH[SexHH==1] <- "Male"
SexHH[SexHH==2] <- "Female"

Under18 <- (AgeHH < 18) #those who are head of house and under 18 years

RelationshipStructure <- IHHDataset$HV217 #Relationship structure (0=no adults, 1=one adult, 2=two adults & opposite sex, 3=two adults  & same sex, 4=three+related adults, 5= unrelated adults)
RelationshipStructure[RelationshipStructure==0] <- "No Adults"
RelationshipStructure[RelationshipStructure==1] <- "One Adult"
RelationshipStructure[RelationshipStructure==2] <- "Two Adults, Opp. Sex"
RelationshipStructure[RelationshipStructure==3] <- "Two Adults, Same Sex"
RelationshipStructure[RelationshipStructure==4] <- "3+ Related Adults"
RelationshipStructure[RelationshipStructure==5] <- "Unrelated Adults"

BankAccount <- (IHHDataset$HV247) #has a bank account (0=no, 1=yes, 9=missing)
BankAccount[BankAccount==0] <- "No"
BankAccount[BankAccount==1] <- "Yes"
BankAccount[BankAccount==9] <- "NA"

WealthIndex <- (IHHDataset$HV270) #wealth index of household (1=poorest, 2=poorer, 3=middle, 4=richer, 5=richest)
WealthIndex[WealthIndex==1] <- "Poorest"
WealthIndex[WealthIndex==2] <- "Poorer"
WealthIndex[WealthIndex==3] <- "Middle"
WealthIndex[WealthIndex==4] <- "Richer"
WealthIndex[WealthIndex==5] <- "Richest"

#----------------------------------------------------------------------------------------------------
####START HERE WITH INDIVIDUAL NON-HOUSEHOLD MEMBER VARIABLES######

SexofHHmember <- IHHDataset$HV104 #sex of household member taking the survey
SexofHHmember[SexofHHmember==1] <- "Male"
SexofHHmember[SexofHHmember==2] <- "Female"

AgeofHHmember <- IHHDataset$HV105 #age of household member taking the survey
Under18HHmember <- AgeofHHmember <18 #age of household members under 18 years

FemaleHHMembers <- SexofHHmember[SexofHHmember=="Female"] #female household members only
FemaleUnder18.subset.data <- subset(IHHDataset, AgeofHHmember < 18 & SexofHHmember=="Female")

Marital2.0 <- FemaleUnder18.subset.data$HV116 #subset of women who are female, under 18 years, and marital status
Marital2.0[Marital2.0==0] <- "Never Married"
Marital2.0[Marital2.0==1] <- "Currently Married"
Marital2.0[Marital2.0==2] <- "Formerly/Ever Married"
Marital2.0[Marital2.0==9] <- "Missing Data"

#make a subset that is just women who are under 18 years and married (both currently and formerly/ever)
ChildBrides.subset.data <- subset(FemaleUnder18.subset.data, Marital2.0=="Currently Married")

HighestEduc <- (IHHDataset$HV106) #highest level of education attained (0=no educat, 1=primary, 2=secondary, 3=higher, 9=dont know)
HighestEduc[HighestEduc==0]<- "No Educ"
HighestEduc[HighestEduc==1]<- "Primary"
HighestEduc[HighestEduc==2]<- "Secondary"
HighestEduc[HighestEduc==3]<- "Higher"
HighestEduc[HighestEduc==9]<- "Don't Know"

HighestEduc.Child <- (ChildBrides.subset.data$HV106) #highest level of education attained of just child bride subset data
HighestEduc.Child[HighestEduc.Child==0]<- "No Educ"
HighestEduc.Child[HighestEduc.Child==1]<- "Primary"
HighestEduc.Child[HighestEduc.Child==2]<- "Secondary"
HighestEduc.Child[HighestEduc.Child==3]<- "Higher"
HighestEduc.Child[HighestEduc.Child==9]<- "Don't Know"

HighestEducYear <- (IHHDataset$HV107) #highest year of education completed (in numeric) [0=no years completed in HV106]#?
HighestEducYear.Child <- (ChildBrides.subset.data$HV107) #highest year of education completed (in numeric) of just child bride subset data #?

EducAttain <- (IHHDataset$HV109) #educational attainment (0=no educ, 1=incomplete primary, 2=complete primary, 3=incomplete secondary, 4=complete secondary, 5=higher, 8=don't know)
EducAttain[EducAttain==0] <- "No Educ"
EducAttain[EducAttain==1] <- "Incomplete Primary"
EducAttain[EducAttain==2] <- "Complete Primary"
EducAttain[EducAttain==3] <- "Incomplete Secondary"
EducAttain[EducAttain==4] <- "Complete Secondary"
EducAttain[EducAttain==5] <- "Higher"
EducAttain[EducAttain==8] <- "Dont Know"

EducAttain.Child <- (ChildBrides.subset.data$HV107) #educational attainment for only child brides subset data
EducAttain.Child[EducAttain.Child==0] <- "No Educ"
EducAttain.Child[EducAttain.Child==1] <- "Incomplete Primary"
EducAttain.Child[EducAttain.Child==2] <- "Complete Primary"
EducAttain.Child[EducAttain.Child==3] <- "Incomplete Secondary"
EducAttain.Child[EducAttain.Child==4] <- "Complete Secondary"
EducAttain.Child[EducAttain.Child==5] <- "Higher"
EducAttain.Child[EducAttain.Child==8] <- "Dont Know"

EverAttendSchool <- (IHHDataset$SH09) #ever attended school (1=Yes, 2=No)
EverAttendSchool[EverAttendSchool==1] <- "Yes"
EverAttendSchool[EverAttendSchool==2] <- "No"

EverAttendSchool.Child <- (ChildBrides.subset.data$SH09)
EverAttendSchool.Child[EverAttendSchool.Child==1] <- "Yes"
EverAttendSchool.Child[EverAttendSchool.Child==2] <- "No"

SchoolAttendStatus <- (IHHDataset$HV129) #school attendance status (0=never attended, 1=entered school, 2=advanced, 3=repeating, 4=dropout, 5=left school 2+ years ago, 8=dont know)
SchoolAttendStatus[SchoolAttendStatus==0] <- "Never Attended"
SchoolAttendStatus[SchoolAttendStatus==1] <- "Entered School"
SchoolAttendStatus[SchoolAttendStatus==2] <- "Advanced"
SchoolAttendStatus[SchoolAttendStatus==3] <- "Repeating"
SchoolAttendStatus[SchoolAttendStatus==4] <- "Dropout"
SchoolAttendStatus[SchoolAttendStatus==5] <- "Left 2+ Yrs Ago"
SchoolAttendStatus[SchoolAttendStatus==8] <- "Dont Know"

SchoolAttendStatus.Child <- (ChildBrides.subset.data$HV129) #school attendance status for child bride subset data
SchoolAttendStatus.Child[SchoolAttendStatus.Child==0] <-"Never Attended"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==1] <-"Entered School"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==2] <-"Advanced"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==3] <-"Repeating"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==4] <-"Dropout"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==5] <-"Left 2+ Yrs Ago"
SchoolAttendStatus.Child[SchoolAttendStatus.Child==8] <-"Dont Know"

Working <- (IHHDataset$SH13) #currently working (0=no, 1=yes)
Working[Working==0] <- "No"
Working[Working==1] <- "Yes"
Working[Working==9] <- "NA"

Working.Child <- (ChildBrides.subset.data$SH13) #currently working for child bride sutbset data 
Working.Child[Working.Child==0] <- "No"
Working.Child[Working.Child==1] <- "Yes"
Working.Child[Working.Child==9] <- "NA"


