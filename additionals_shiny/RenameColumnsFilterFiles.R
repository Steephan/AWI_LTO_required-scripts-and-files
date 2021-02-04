##   2020-09-14 CL change of wording: "created" ==> "flagging_date", "creator" ==> "flagged_by"

bla <- list.files(filterbasepath)

for (i in 3:length(bla)) {
	dummy <- read.table(file = paste(filterbasepath, bla[i], sep = ""), sep = ",", dec = ".", header = TRUE)
	names(dummy) <- sub(pattern = "created", replacement = "flagging_date", x = names(dummy))
	names(dummy) <- sub(pattern = "creator", replacement = "flagged_by", x = names(dummy))
	write.table(x = dummy, file = paste(filterbasepath, bla[i], sep = ""), sep = ",", dec = ".", row.names = FALSE)
}