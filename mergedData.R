load_files <- sub("Rout.fake", "RData", input_files)

load(load_files[[1]])
New <- Answers

for (f in 2:length(load_files)){
	load(load_files[[f]])
	New <- rbind(New, Answers)
}

Answers <- New

attach(Answers)

CondomTable<-table(condomLastTime)
prop.table(CondomTable)

knoHIVTable<-table(knowsPersonHIV)
prop.table(knoHIVTable)
summary(Answers)

table(Answers$whoLastSex)
table(Answers$religion)
table(Answers$ageFirstSex)


# rdsave(Answers)
