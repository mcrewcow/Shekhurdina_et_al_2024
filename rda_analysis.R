library(vegan)

rdaplot <- read.table(file = "clipboard", sep = "\t", header=TRUE)


rownames(rdaplot) <- rdaplot$Parameter
rdaplot <- rdaplot[,-1]
dca <- decorana(rdaplot)
data.log <- log1p(rdaplot)
data.hell <- decostand(data.log, 'hell')
predictors = paste(colnames(data.hell), collapse=" + ")
formula = as.formula(paste("data.hell ~ ", predictors))
tbRDA <- rda(formula, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
text(fig, 'species', col = 'red', cex = 0.8)
fig 


plot(dca)
summary(dca)
summary(tbRDA)
eigenvalues <- tbRDA$CA$eig
total_variance <- sum(eigenvalues)
variance_explained_RDA1 <- eigenvalues[1] / total_variance
variance_explained_RDA2 <- eigenvalues[2] / total_variance
print(paste("Variance explained by RDA1:", variance_explained_RDA1 * 100, "%"))
print(paste("Variance explained by RDA2:", variance_explained_RDA2 * 100, "%"))
anova_result <- anova.cca(tbRDA, permutations = 999)
print(anova_result)
anova_result <- anova.cca(tbRDA, permutations = 999, by = 'term')
print(anova_result)
