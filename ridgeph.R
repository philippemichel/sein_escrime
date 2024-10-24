
zz <- tt |> 
  dplyr::select(36:37) |> 
  drop_na() |> 
  pivot_longer(1:2) |> 
  rename(Score = name) |> 
  mutate(Score = factor(Score, labels = c("Score moyen mental","Score moyen physique"))) 
  
  
ridgeph <- function(df, titre = ""){
 df |>  
  ggplot() +
    aes(x= value, y = Score, fill = Score) +
    geom_density_ridges() +
  theme_ridges() +
    geom_vline(xintercept = 0,linetype = "dotdash", color = "grey60") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
    labs(title = titre) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 12 ),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )
} 
    