df <- read.csv("dataset.csv")
col_names <- t(t(colnames(df)))

satisfaction_sample <- df$Are.you.satisfied.with.the.overall.e.learning.experience.
barplot(table(satisfaction_sample), xlab="Satisfaction level", ylab="Count")
sample_mean <- mean(df$Are.you.satisfied.with.the.overall.e.learning.experience)
paste("Sample mean - ", sample_mean)

no_of_bootstrap_samples <- 1000
sample_size <- length(satisfaction_sample)
bootstrap_means <- c()

for (i in 1:no_of_bootstrap_samples) {
  bootstrapSample <- sample(satisfaction_sample, size = sample_size, replace = TRUE)
  bootstrap_means[i] <- mean(bootstrapSample)
}

paste("Bootstrap s.d.", sd(bootstrap_means))
margin_error <- sd(bootstrap_means) * 2

paste("Confidence interval - (", sample_mean - margin_error, ",", sample_mean + margin_error, ")")

plot(density(bootstrap_means), xlab = "Bootsrap mean distribution", )