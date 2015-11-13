Answers <- within(Answers, {
	levels(province)[grepl("Th", levels(province))] <- "Thies"
})
