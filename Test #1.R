WAdatabase <- read.csv("WA_ESSDatabase.csv", header = TRUE)

# var1 <- "hello world!"
# arr1 <- c("hello!", "my", "name", "is", "Simão.")
# 
# print(var1)
# 
# for (string in arr1) {
#   print(string)
# }


total_count <- nrow(WA_ESSdatabase)
behaviour_count <- length(WA_ESSdatabase[WA_ESSdatabase$ipbhprp == 2, 'ipbhprp'])
print(behaviour_count / total_count)
