

#  ------------------------------------------------------------------------
#
# Title : import
#    By : PhM
#  Date : 2024-03-16
#    
#  ------------------------------------------------------------------------


library(tidyverse)
library(janitor)
library(readODS)
#

bn <- read_ods("data/escrime2.ods", sheet= "bnom")

tt <- read_ods("data/escrime2.ods", sheet= "data", col_types = NULL, na = c("ND","NA")) |> 
  janitor::clean_names() |> 
  mutate_if(is.character, as.factor) |> 
  mutate(id = as.factor(id)) |>
## Recodage de tt$distance_k en tt$distance_k
mutate(distance_k = cut(distance_k,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 3, 6, 12, 24, 100),
  labels = c("< 3", "3 - 6", "6 - 12", "12 - 24", "> 24")
)) |> 
## Recodage de tt$tumo_mast
mutate(tumo_mast =
  fct_recode(tumo_mast,
    "mastectomie" = "mast",
    "tumorectomie" = "tumo"
  ))
  var_label(tt) <- bn$Nom
tt <- tt |> 
  mutate(sf36p_dif = s_physique_20_seances - s_physique_avant) |> 
  mutate(sf36m_dif = s_mental_20_seances - s_mental_avant) |> 
  ## Recodage de tt$grabataire_sportive en tt$grabataire_sportive_rec
mutate(grabataire_sportive = cut(grabataire_sportive,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 4, 5, 8, 10), 
  labels = c("0 - 4","4 - 5", "6 - 7", ">7")
))
#
var_label(tt$sf36p_dif) <- "SF36 physique: évolution"
var_label(tt$sf36m_dif) <- "SF36 mental : évolution"
saveRDS(tt, "data/escrime.rds")
saveRDS(bn, "data/bn.rds")
