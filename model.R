attach(Answers)
options("width"=200)
library(lme4)
library(coefplot2)
library(gdata)
library(splines)

# Make a special data frame for this model

predNames <- c( "knowsPersonHIV", "knowsCondomProtect")

modNames <- c("age", "gender", "urbanRural", "educLevel",  "wealthRaw", "MrtStat", "religion", "AIDSlookHealthy", "whoLastSex")
factNames <- c("province", "clusterId", "dataset")
allNames <- c(predNames, modNames, factNames, "condomLastTime")

ModAns <- na.omit(Answers[allNames])

null_model <- glmer(
	condomLastTime
	~ ns(age, 4)
	+ ns(wealthRaw, 4)
	+ gender
	+ urbanRural
	+ educLevel
	+ religion 
	+ MrtStat
	+ AIDSlookHealthy
	+ whoLastSex
	+ (1|dataset:clusterId)
	+ (1|dataset:province)
	+ (1|dataset), 

	family="binomial", data=ModAns
) 

knows_model <- update(null_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

avoid_model <- update(null_model, . ~
	. + knowsCondomProtect + (0 + knowsCondomProtect | dataset))

base_model <- update(avoid_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

interaction_model <- update(base_model, . ~ . +  knowsPersonHIV:knowsCondomProtect + (0 + knowsPersonHIV:knowsCondomProtect|dataset))

drop1(base_model, test="Chisq")

###################### Base Model ###############################
lme4:::printMer(summary(base_model),correlation=FALSE)

############ Knowledge x PWA Interaction ######################
lme4:::printMer(summary(interaction_model),correlation=FALSE)

### Overall effect without interaction###
anova(base_model, null_model)

### Overall effect of interaction###
anova(base_model, interaction_model)

### Effect of "avoid" in base model ###
# Compare interaction model by dropping two "avoid" terms
anova(knows_model, base_model)

### Effect of "knows" in base model ###
# As above
anova(avoid_model, base_model)

# rdsave(base_model, ModAns)

