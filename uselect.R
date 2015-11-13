cat("######################  ########################\n\n")

cat(rtargetname)

raw <- readLines(input_files[[1]])
sel <- grep("^[^#]", raw, value=TRUE)

Q <- attr(Data, 'variable.labels')
names(Q) = sub('[$]', '.', names(Q))

Answers <- data.frame(row.names=row.names(Data))
Questions <- character(0)

for (s in sel){
	l <- scan(text=s, sep=",", what="character",
		strip.white=TRUE, quiet=TRUE
	)

	new <- l[[1]]

	if (new %in% names(Answers)){
		stop("Replicated new name ", s, " in .csv")
	}

	pick <- intersect(l[-1], names(Data))
	if (length(pick) > 1) {stop ("Multiple matches to ", s, "in .csv")}
	if (length(pick)==1) {
		Answers[[new]] <- Data[[pick]]
		Questions[[new]] <- Q[[pick]]
	}
	if (length(pick)==0){
		Questions[[new]] <- NA
		Answers[[new]] <- NA
		print(paste("Did not find variable ", s, "in .csv"))
	}
}

print(names(Answers))

print(data.frame(Question=Questions))

# rdsave(Answers, Questions)
