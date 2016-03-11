# Generate example dataset for shiny app


# sample(10, 20, replace = TRUE)

set.seed(1)
df.expr <- data.frame(matrix(sample(100, 50, replace = TRUE), ncol = 5))
rownames(df.expr) <- c("sdg", "dhdrhd", "lsiegseg", "shedrhdh", "drhdrhd54e", "Msdhe4", "Masdfasd", "44u4njdEfGd", "Lasdfg02", "nfgna1")
colnames(df.expr) <- c("asldfsdf", "ccasfdlckjf", "ccag", "cchsfagsd", "ccjdfgli")
save(df.expr, file = "df.expr.RData")




# lines(x = 1:length(df.expr[1, ]), y = df.expr[which(rownames(df.expr) == "ag"), ])

