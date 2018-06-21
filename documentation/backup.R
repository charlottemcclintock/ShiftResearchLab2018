test <- tapply(df$`Chief Executives`, df$nhname, summary)

test <- data.frame(test)

test <- separate(test, test, c("class", "false", "true"), sep = ",")
test$true <- ifelse(is.na(test$true), test$false, test$true)

test <- separate(test, false, c("bool1", "false"), sep = "=")
test <- separate(test, true, c("bool2", "true"), sep = "=")
test <- separate(test, bool1, c("ticka","bool1", "tickb"), sep = "`")
test <- separate(test, bool2, c("tickc","bool2", "tickd"), sep = "`")
test <- separate(test, false, c("quo1","false", "quo2"))
test <- separate(test, true, c("quo1","true", "quo2"))

test$false <- ifelse(test$bool1=="TRUE", 0, test$false)

test <- select(test, true, false)

test <- cbind(rownames(test), test)
names(test) <- c("nbhd", "true", "false")

test <- mutate(test,
               true=as.numeric(true),
               false=as.numeric(false),
               total=true+false, 
               affordability=100*true/total,
               occupation="Chief Executives")

test <- select(test, nbhd, affordability)