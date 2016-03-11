# Generate example dataset for shiny app




set.seed(100)


# generate values for data frame ("expression values")
df.expr <- data.frame(matrix(sample(10000, 50000*5, replace = TRUE), ncol = 5))


# generate row names ("genes")
r.names <- sample(letters, 50000, replace = TRUE)
for(i in 1:20) {
    r.names <- paste(r.names, sample(letters, 50000, replace = TRUE), sep = "")
}



rownames(df.expr) <- r.names
colnames(df.expr) <- c("asldfsdf", "ccasfdlckjf", "ccag", "cchsfagsd", "ccjdfgli")
save(df.expr, file = "df.expr.RData")




