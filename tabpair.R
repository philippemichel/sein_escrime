

ncol <- c(24,34)

tabpair <- function(ncol, exp = FALSE, classeur = classeur, sheet){
noms = c()
tabx <- NULL
for (i in ncol) {
  print(i)
  lig <- NULL
  t1 <- pull(tt[,i])
  t2 <- pull(tt[,i+1])
  med1 <- paste0(round(median(t1, na.rm = TRUE),1)," (", round(quantile(t1,na.rm = TRUE)[[2]],1)," - ",round(quantile(t1,na.rm = TRUE)[[4]],1),")") 
  med2 <- paste0(round(median(t2, na.rm = TRUE),1)," (", round(quantile(t2,na.rm = TRUE)[[2]],1)," - ",round(quantile(t2,na.rm = TRUE)[[4]],1),")") 
  dif <- wilcox.test(t1,t2, paired = TRUE)
  lig <- c(med1,med2,beaup(dif$p.value))
  tabx <- rbind(tabx,lig)
}
taby <- as_tibble(tabx) |> 
mutate(Score = c("Score physique", "Score mental")) |> 
  relocate(Score) 
names(taby) <- c("SF36","Avant","20 séances","p-value")
zz <- kbl(taby, booktabs = TRUE) |>
  kable_styling(latex_options = c("HOLD_position")) |>
  footnote(general = "Médiane (quartiles) - Test de Wilocoxon")
if (exp) {
  zz |>
    xlsx::write.xlsx(classeur, sheetName = sheet, append = TRUE)
}
zz
}

