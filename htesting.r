df <- read.csv("dataset.csv")
col_names <- t(t(colnames(df)))

## Overall preference

overall_preference <- df$What.do.you.prefer.if.students.are.allowed.to.engage.in.undergraduate.studies.using.both.means.e.learning.and.traditional.method
overall_elearn <- length(which(overall_preference=="E-learning"))
overall_traditional <- length(which(overall_preference=="Traditional method"))
overall_se <- sqrt(0.5*0.5/(overall_elearn + overall_traditional))
overall_t <- ((overall_elearn/ (overall_elearn + overall_traditional)) - 0.5)/ overall_se
overall_p <- pt(q=overall_t, df=(overall_elearn + overall_traditional - 1), lower.tail=FALSE)

if (overall_p < 0.05) {
  paste("We can reject null hypothesis")
} else {
  paste("We cannot reject null hypothesis")
}

## Practical preference

practical_preference <- df$What.is.the.best.method.for.you.to.learn.practical.knowledge.
practical_elearn <- length(which(practical_preference=="E-learning (Online practical sessions and and online learning platform)"))
practical_traditional <- length(which(practical_preference=="Physically attending practical sessions"))
practical_se <- sqrt(0.5*0.5/(practical_elearn + practical_traditional))
practical_t <- ((practical_traditional/ (practical_elearn + practical_traditional)) - 0.5)/ practical_se
practical_p <- pt(q=practical_t, df=(practical_elearn + practical_traditional -1), lower.tail=FALSE)

if (practical_p < 0.05) {
  paste("We can reject null hypothesis")
} else {
  paste("We cannot reject null hypothesis")
}


## Practical preference using bootstrap

