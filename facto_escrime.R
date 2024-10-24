library(missMDA)
library(FactoMineR)
library(factoextra)

tta <- tt |> 
  dplyr::select(c(1:6,9:10,31,32,19,29)) |> 
imputeFAMD(ncp =1) 
 

tta <- as_tibble(tta$completeObs) |> 
  mutate_if(is.factor, as.numeric) 

fviz_screeplot(rtt, addlabels = TRUE, ylim = c(0, 30))

rtt <- PCA(tta)
fviz_pca_var(rtt,repel = TRUE,select.var = list(contrib = 10), col.var = "cos2")

fviz_pca_ind(rtt, geom = "point",  addEllipses = TRUE, habillage = as.factor(tta$chimio))
