# Check if a LIB exist, if not, install it
check_lib <- function(mypkg, install_pkg=1){
  if(is.element(mypkg, installed.packages()[,1])){
    suppressMessages(library(mypkg, character.only=TRUE))
  } else {
    if(install_pkg == 1){
      install.packages(mypkg, character.only=TRUE, repos="http://vps.fmvz.usp.br/CRAN/")
      suppressMessages(library(mypkg, character.only=TRUE))
    } else {
      stop(paste("Missing package:", mypkg, sep=" "), call.=FALSE)
    }
  }
}
# Load libs or install if it not exist
check_lib("prcomp")
check_lib("tidyverse") 
check_lib("plotly") 
check_lib("heatmaply")
check_lib("devtools") 

# Load data
pan_matrix <- matrix("Orthologous.csv")

pan_dist <- dist.alignment(pan_matrix, matrix = "identity")
habillage <- datas$group[1:nrow(res.pca$x)]
res.pca <- prcomp(as.matrix(pan_matrix), scale = TRUE)



# Color individuals by groups
fviz_pca_ind(res.pca, label="none", habillage=habillage)
fviz_eig(res.pca)
fviz_pca_ind(res.pca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE)

fviz_pca_ind(res.pca,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             
fviz_pca_biplot(res.pca, label="var", habillage=habillage,
                             addEllipses=TRUE, ellipse.level=0.8)
library("corrplot")
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE)
