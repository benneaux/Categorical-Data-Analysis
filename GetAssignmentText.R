library(tabulizer)
library(tm)
library(stringi)

file_loc <- "Handouts/Assignment 1.pdf"
split_text_question <- "Question"
split_text_part <- "\\n[iv]+\\)"
df <- as.data.frame(extract_text(file_loc))
df <- stri_list2matrix(stri_split_fixed(df[,1] , split_text_question))
df2 <- t(stri_list2matrix(stri_split_regex(df[,1],split_text_part)))


write.csv(df2, "Handouts/Assignment1.csv")
