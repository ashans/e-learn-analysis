df <- read.csv("dataset.csv")
col_names <- t(t(colnames(df)))

practical_preference <- df$What.is.the.best.method.for.you.to.learn.practical.knowledge.
practical_elearn <- length(which(practical_preference=="E-learning (Online practical sessions and and online learning platform)"))
practical_traditional <- length(which(practical_preference=="Physically attending practical sessions"))

practical_both <- c(rep("E", practical_elearn), rep("T", practical_traditional))


no_of_bootstrap_samples <- 1000
sample_size <- length(practical_both)
sample_proportion <- practical_traditional / (practical_traditional + practical_elearn)
bootstrap_proportions <- c()
count <- 0
for (i in 1:no_of_bootstrap_samples) {
  bootstrapSample <- sample(practical_both, size = sample_size, replace = TRUE)
  bootstrap_proportions[i] <- length(which(bootstrapSample == "T")) / sample_size
  if(bootstrap_proportions[i] > 0.5) {
    count <- count + 1
  }
}

proportion <- count / no_of_bootstrap_samples

if (proportion > 0.95) {
  
  "reject null"
  
}else {
  
  "do not reject null"
}

hist(bootstrap_proportions)