
cat("################################################")

cat(rtargetname)

if (grepl("mr.", rtargetname)){gender <- "M"}
if (grepl("ir.", rtargetname)){gender <- "F"}
print(gender)

library(gdata)
library(splines)



Answers <- within(Answers, {
	respondent <- as.character(respondent)
	reasNoCont <- as.factor(reasNoCont)
	everSex <-ifelse(ageFirstSex==0, "No", "Yes")
	everSex <- as.factor(everSex)
	levels(everSex)[levels(everSex)=="No"] <-NA
	levels(heardAIDS)[levels(heardAIDS)=="No"] <-NA
	sex12mos <-timeSinceSex<400
	sex12mos <- as.factor(sex12mos)
	levels(sex12mos)[levels(sex12mos )=="FALSE"] <-NA
	clusterId <- as.factor(clusterId)
	#stndrd_age=scale(age,center=TRUE,scale=TRUE)
	knowsCondomProtect[knowsCondomProtect=="Don't know"] <- "No"
	levels(knowsCondomProtect)[levels(knowsCondomProtect)=="Reduce chances of AIDS by always using condoms during sex"] <- "Yes"
	knowsCondomProtect <-drop.levels(knowsCondomProtect, reorder=FALSE)
	levels(condomLastTime)[levels(condomLastTime)=="Yes, female condom"] <- "Yes"
	levels(condomLastTime)[levels(condomLastTime)=="Yes, male condom"] <- "Yes"
	condomLastTime[condomLastTime=="DK"] <- NA 
	condomLastTime <- drop.levels(condomLastTime, reorder=FALSE)
	knowsPersonHIV <- drop.levels(knowsPersonHIV, reorder=FALSE)
	ageFirstSex[ageFirstSex=="98"] <- NA
	urbanRural <- drop.levels(urbanRural, reorder=FALSE)
	educLevel <- drop.levels(educLevel, reorder=FALSE)
	religion <- drop.levels(religion, reorder=FALSE)
	levels(AIDSlookHealthy )[levels(AIDSlookHealthy )=="No"] <- "No/DK"
	levels(AIDSlookHealthy )[levels(AIDSlookHealthy )=="DK"] <- "No/DK"
	AIDSlookHealthy <- drop.levels(AIDSlookHealthy, reorder=FALSE)

		levels(whoLastSex)[levels(whoLastSex)=="Spouse"] <- "Cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Boyfriend not living with respondent"] <- "Non-cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Casual acquaintance"] <- "Other/casual"
		levels(whoLastSex)[levels(whoLastSex)=="Live-in partner"] <- "Cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Other"] <- "Other/casual"
		levels(whoLastSex)[levels(whoLastSex)=="Relative"] <- "Other/casual"
		levels(whoLastSex)[levels(whoLastSex)=="Spouse/cohabiting partner"] <- "Cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Boyfriend/fiance"] <- "Non-cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Other friend"] <- "Other/casual"
		levels(whoLastSex)[levels(whoLastSex)=="Unmarried partner not living with respondent"] <- "Cohabiting partner"
		levels(whoLastSex)[levels(whoLastSex)=="Girlfriend/fiancee"] <- "Non-cohabiting partner"


	whoLastSex <- drop.levels(whoLastSex, reorder=FALSE)

		levels(religion)[levels(religion)=="Catholic"] <- "Catholic/Orthodox"
		levels(religion)[levels(religion)=="catholic"] <- "Catholic/Orthodox"
		levels(religion)[levels(religion)=="Roman catholic"] <- "Catholic/Orthodox"
		levels(religion)[levels(religion)=="Roman Catholic"] <- "Catholic/Orthodox"
		levels(religion)[levels(religion)=="Roman Catholic church"] <- "Catholic/Orthodox"

		levels(religion)[levels(religion)=="Adventist"] <- "Other Christian"
		levels(religion)[levels(religion)=="Anglican Church"] <- "Other Christian"
		levels(religion)[levels(religion)=="Apostolic Sect"] <- "Other Christian"
		levels(religion)[levels(religion)=="Apostolic sect"] <- "Other Christian"
		levels(religion)[levels(religion)=="Lesotho Evangelical church"] <- "Other Christian"
		levels(religion)[levels(religion)=="Methodist"] <- "Other Christian"
		levels(religion)[levels(religion)=="Charismatic"] <- "Other Christian"
		levels(religion)[levels(religion)=="Zionist"] <- "Other Christian"
		levels(religion)[levels(religion)=="Pentecostal"] <- "Other Christian"
		levels(religion)[levels(religion)=="protestant"] <- "Other Christian"
		levels(religion)[levels(religion)=="Protestant"] <- "Other Christian"
		levels(religion)[levels(religion)=="Christian"] <- "Other Christian"
		levels(religion)[levels(religion)=="Protestant/ other Christian"] <- "Other Christian"
		levels(religion)[levels(religion)=="SDA"] <- "Other Christian"
		levels(religion)[levels(religion)=="Seventh Day Adventist"] <- "Other Christian"

		levels(religion)[levels(religion)=="Islam"] <- "Muslim"

		levels(religion)[levels(religion)=="Animist"] <- "None/Other"
		levels(religion)[levels(religion)=="Traditional"] <- "None/Other"
		levels(religion)[levels(religion)=="NONE"] <- "None/Other"
		levels(religion)[levels(religion)=="None"] <- "None/Other"
		levels(religion)[levels(religion)=="No religion"] <- "None/Other"
		levels(religion)[levels(religion)=="Other"] <- "None/Other"
		levels(religion)[levels(religion)=="Others"] <- "None/Other"

		gender <- factor(gender, levels=c("F", "M"))

})



Answers <- subset(Answers, !is.na(everSex)) 
Answers <- subset(Answers, !is.na(sex12mos)) 
Answers <- subset(Answers, !is.na(whoLastSex))
Answers <- subset(Answers, !is.na(condomLastTime))
Answers <- subset(Answers, !is.na(heardAIDS))
Answers <- subset(Answers, !is.na(AIDSlookHealthy))
Answers <- subset(Answers, !is.na(knowsPersonHIV)) 
Answers <- subset(Answers, !is.na(knowsCondomProtect)) 
Answers <- subset(Answers, !is.na(religion)) 
Answers <- subset(Answers, !is.na(MrtStat)) 

summary(Answers)
