library(FactoMineR)
library(factoextra)
df <- iris[, !(names(iris) %in% c("Species"))]
pca <- PCA(df, scale.unit = TRUE, ncp = 5, graph = FALSE)
fviz_pca_ind(pca, geom.ind = "point",col.ind = iris$Species,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Groups"
)
fviz_pca_ind(pca, geom.ind = "point",col.ind = iris$Species,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Groups"
)

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_eig(pca, addlabels = TRUE)

library(corrplot)
variables <- get_pca_var(pca)
head(variables$coord)
head(variables$cos2)
corrplot(variables$cos2, is.corr = FALSE)

fviz_contrib(pca, choice = "var")
fviz_contrib(pca, choice = "var", axes = 2)
fviz_contrib(pca, choice = "var", axes = 3)
fviz_contrib(pca, choice = "var", axes = 4)