df <- read.csv("dataset.csv")
col_names <- t(t(colnames(df)))

columns <- c(1,3,6,7,13,14)
for (col in columns) {
  barplot(table(df[col]), xlab=col_names[col], ylab="Count")
}