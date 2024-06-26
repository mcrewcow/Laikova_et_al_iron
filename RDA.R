library(vegan)

rdaplot <- read.table(file = "clipboard", sep = "\t", header=TRUE)
View(rdaplot)
rownames(rdaplot) <- rdaplot$Material
rdaplot <- rdaplot[,-1]
data.log <- log1p(rdaplot)
data.hell <- decostand(data.log, 'hell')
tbRDA <- rda(data.hell ~ H2.ase + K..mL.H2.g.COD.d + Fe2. + Fe3. + γ..mL.H2.g.COD
             + λ..d + Т90...d, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

tbRDA <- rda(data.hell ~ ECE... + X..Н2..CODinit + X..Н2..CODMP, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

tbRDA <- rda(data.hell ~ Concentration..mg.L + Reactor.volume..mL + HY..mL.g.COD +
               pH + Time..h + Enhancement... + Substrate.concentration..g.COD.L, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
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
