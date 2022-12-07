install.packages(c("FactoMineR", "factoextra"))

library(FactoMineR)
library(factoextra)

data(decathlon2)
# head(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

library("FactoMineR")
res.pca <- PCA(decathlon2.active, graph = FALSE)

print(res.pca)

library("factoextra")
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

# Coordonnées des variables
head(var$coord, 4)

fviz_pca_var(res.pca, col.var = "black")

head(var$cos2, 4)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

# Changer la transparence en fonction du cos2
fviz_pca_var(res.pca, alpha.var = "cos2")

head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# Changez la transparence en fonction de contrib
fviz_pca_var(res.pca, alpha.var = "contrib")

# Créer une variable aléatoire continue de longueur 10
set.seed (123)
my.cont.var <- rnorm (10)

# Colorer les variables en fonction de la variable continue
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

# Créez une variable de regroupement en utilisant kmeans
# Créez 3 groupes de variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Colorer les variables par groupes
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc$Dim.1

# Description de la dimension 2
res.desc$Dim.2

ind <- get_pca_ind(res.pca)
ind

# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

fviz_pca_ind (res.pca)

fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)


fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Évite le chevauchement de texte
)


fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

fviz_cos2(res.pca, choice = "ind")

# Contribution totale sur PC1 et PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

# Créez une variable continue aléatoire de longueur 23,
# Même longeur que le nombre d'individus actifs dans l'ACP
set.seed (123)
my.cont.var <- rnorm(23)
# Colorer les individus par la variable continue
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

head(iris, 3)

# La variable Species (index = 5) est supprimée
# avant l'ACP
iris.pca <- PCA(iris [, - 5], graph = FALSE)

fviz_pca_ind(iris.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = iris$Species, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)


# Ajoutez des ellipses de confiance
fviz_pca_ind(iris.pca, geom.ind = "point", col.ind = iris$Species, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)


fviz_pca_ind(iris.pca,
             label = "none", # Caché le texte des individus
             habillage = iris$Species, # colorer par groupes
             addEllipses = TRUE, # Ellipses de concentration
             palette = "jco"
)


# Variables sur les dimensions 2 et 3
fviz_pca_var(res.pca, axes = c(2, 3))
# Individus sur les dimensions 2 et 3
fviz_pca_ind(res.pca, axes = c(2, 3))


# Afficher les points et l'annotation des variables
fviz_pca_var(res.pca, geom.var = c("point", "text"))

# Afficher uniquement l'annotation des individus
fviz_pca_ind(res.pca, geom.ind = "text")

# Changez la taille des flèches et du texte
fviz_pca_var(res.pca, arrowsize = 1, labelsize = 5,
             repel = TRUE)
# Modification de la taille, de la forme 
# et de la couleur de remplissage des points
# Modifier la taille du texte
fviz_pca_ind (res.pca,
              pointsize = 3, pointshape = 21, fill = "lightblue",
              labelsize = 5, repel = TRUE)

# Add confidence ellipses
fviz_pca_ind(iris.pca, geom.ind = "point", 
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
# Convex hull
fviz_pca_ind(iris.pca, geom.ind = "point",
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
)

fviz_pca_ind (iris.pca,
              geom.ind = "point", # afficher les points seulement (pas de "texte")
              col.ind = iris$Species, # Couleur par groupes
              legend.title = "Groupes",
              mean.point = FALSE)

fviz_pca_var (res.pca, axes.linetype = "blank")

ind.p <- fviz_pca_ind(iris.pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "Iris data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)


fviz_pca_biplot (iris.pca,
                 col.ind = iris$Species, palette = "jco",
                 addEllipses = TRUE, label = "var",
                 col.var = "black", repel = TRUE,
                 legend.title = "Species")

fviz_pca_biplot(iris.pca, 
                # Colueur de remplissage des individdus par groupes
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                # Colorer les variables par groupes
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        # Evite le chévauchement du texte
)+
  ggpubr::fill_palette("jco")+      # Couleur des individus
  ggpubr::color_palette("npg")      # Couleur des variables

fviz_pca_biplot(iris.pca, 
                # Individus
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

PCA(X, ind.sup = NULL, 
    quanti.sup = NULL, quali.sup = NULL, graph = TRUE)

res.pca <- PCA(decathlon2, ind.sup = 24:27, 
               quanti.sup = 11:12, quali.sup = 13, graph=FALSE)

res.pca$quanti.sup

fviz_pca_var(res.pca)

# Changer la couleur des variables
fviz_pca_var(res.pca,
             col.var = "black", # Variables actives
             col.quanti = "red" # variables quantitatives supl.
)

# Cacher les variables actives sur le graphique,
# ne montrent que des variables supplémentaires
fviz_pca_var(res.pca, invisible = "var")

# Cacher les variables supplémentaires
fviz_pca_var(res.pca, invisible = "quanti.sup")

# Graphique des variables actives
p <- fviz_pca_var(res.pca, invisible = "quanti.sup")
# Ajouter des variables actives supplémentaires
fviz_add(p, res.pca$quanti.sup$coord, 
         geom = c("arrow", "text"), 
         color = "red")

res.pca$ind.sup

p <- fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)
p <- fviz_add(p, res.pca$quali.sup$coord, color = "red")
p


res.pca$quali


fviz_pca_ind(res.pca, habillage = 13,
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

# Visualiser les variables avec cos2> = 0.6
fviz_pca_var (res.pca, select.var = list(cos2 = 0.6))
# Top 5 variables actives avec le cos2 le plus elevé
fviz_pca_var (res.pca, select.var = list(cos2 = 5))
# Sélectionnez par noms
name <- list (name = c ("Long.jump", "High.jump", "X100m"))
fviz_pca_var (res.pca, select.var = name)
# Top 5 des individus/variables les plus contibutifs
fviz_pca_biplot (res.pca, select.ind = list (contrib = 5),
                 select.var = list (contrib = 5),
                 ggtheme = theme_minimal())