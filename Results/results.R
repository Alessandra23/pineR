load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/kilduffData.Rda")

hortlandW$gemtime_new_t0-1095
#dayPredicted <- (hortlandW$gemtime_new_t0-1095)[quantile(hortlandW$gemtime_new_t0-1095, 0.75)]
dayPredicted <- quantile(KilduffDataW$gemtime_new_t0-1095, 0.25)
#quantile(hortlandW$gemtime_new_t0-1095)
dayObserved <- emergence$Kilduff[which.min(abs(emergence$Kilduff$prob - 0.25)), 1]
#dayObserved <- 230
(daysDiff <-  dayPredicted - dayObserved)
daysDiff <- round(daysDiff)

plot(ecdf((hortlandW$gemtime_new_t0-1095)-daysDiff), xlab = "day", ylab = "CDF", main = "Hortland")
lines(emergence$Hortland, col = "red")
plot(ecdf((hortlandW$gemtime_new_t0-1095)), xlab = "day", ylab = "CDF", main = "Hortland")
lines(emergence$Hortland, col = "red")


N = 50
rep <- replicate(N,  runs <- piner(npop = 10, ngen = 1, ntimes = 10, data = KilduffData,
                                   firstrun = 1, species = 1, depth = 3, owp = 100, output = 1,
                                   seed = 001))

#No correction
list_rep <- rep["gemtime_new_t0",]
list_rep <- lapply(list_rep, function(x) (x - 1095))

#list_rep <- apply(HH, 1, function(x) (x - 1095)-3)

#n_row <- range(emer_true$day)
n_row <- range(list_rep)
matrix_edcf <- matrix(NA, ncol = N, nrow = n_row[2]- n_row[1]+1)
for(i in 1:N){
  ecdf_list = ecdf(list_rep[[i]])
  matrix_edcf[,i] = ecdf_list(n_row[1]:n_row[2])
}
matrix_quantile = apply(matrix_edcf, 1, quantile, c(0.025,0.5,0.975))
# quantiles_df <- tibble(day = c(n_row[1]:n_row[2]),
#                        ql = matrix_quantile[1,],
#                        qu = matrix_quantile[3,],
#                        qm = matrix_quantile[2,])

quantiles_df <- tibble(day = seq(n_row[1],n_row[2],1),
                       ql = matrix_quantile[1,],
                       qu = matrix_quantile[3,],
                       qm = matrix_quantile[2,])

depth = 1
emer_true = emergence$Kilduff
site <- "Kilduff"

ecdf_var <- ggplot() +
  geom_line(data = quantiles_df, aes(x = day, y = qm, colour =  "steelblue" ),  size = 1) +
  geom_line(data = emer_true, aes(x = day, y = prob, colour = "firebrick"), size = 1) +
  ylim(0, 1) + scale_color_hue(labels = c("Real", "Simulated"), name = "Emergence") +
  geom_ribbon(data = quantiles_df, aes(x = day, ymin = ql, ymax = qu),
              alpha=0.1,
              linetype="dashed",
              color="grey") +
  labs(x = "Day",
       y = "Probability") +
       #title = paste("Depth = ", depth, "Site" = site))+
       #title = paste("Site" = site))+
  xlim(c(0,350))+
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ "Kilduff")
ecdf_var



## with correction

#No correction
list_rep <- rep["gemtime_new_t0",]
list_rep <- lapply(list_rep, function(x) {(x - 1095)-daysDiff})

#list_rep <- apply(HH, 1, function(x) (x - 1095)-3)

#n_row <- range(emer_true$day)
n_row <- range(list_rep)
matrix_edcf <- matrix(NA, ncol = N, nrow = n_row[2]- n_row[1]+1)
for(i in 1:N){
  ecdf_list = ecdf(list_rep[[i]])
  matrix_edcf[,i] = ecdf_list(n_row[1]:n_row[2])
}
matrix_quantileC = apply(matrix_edcf, 1, quantile, c(0.025,0.5,0.975))
# quantiles_df <- tibble(day = c(n_row[1]:n_row[2]),
#                        ql = matrix_quantile[1,],
#                        qu = matrix_quantile[3,],
#                        qm = matrix_quantile[2,])

quantiles_dfC <- tibble(day = seq(n_row[1],n_row[2],1),
                       ql = matrix_quantileC[1,],
                       qu = matrix_quantileC[3,],
                       qm = matrix_quantileC[2,])


ecdf_varC <- ggplot() +
  geom_line(data = quantiles_dfC, aes(x = day, y = qm, colour =  "steelblue" ),  size = 1) +
  geom_line(data = emer_true, aes(x = day, y = prob, colour = "firebrick"), size = 1) +
  ylim(0, 1) + scale_color_hue(labels = c("Real", "Simulated"), name = "Emergence") +
  geom_ribbon(data = quantiles_dfC, aes(x = day, ymin = ql, ymax = qu),
              alpha=0.1,
              linetype="dashed",
              color="grey") +
  labs(x = "Day",
       y = "Probability") +
  #title = paste("Depth = ", depth, "Site" = site))+
  #title = paste("Site" = site))+
  xlim(c(0,350))+
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ "Kilduff Correction 25%")
ecdf_varC

legend <- get_legend(ecdf_var)

ecdf_var <- ecdf_var + theme(legend.position="none")
ecdf_varC <- ecdf_varC + theme(legend.position="none",
                               #axis.text.y = element_blank(),
                               axis.title.y = element_blank())
                               #axis.ticks = element_blank())
# 4. Create a blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1)) +
  cowplot::theme_nothing()

grid.arrange(ecdf_var, ecdf_varC, legend, ncol=3, nrow = 1,
             widths=c(2.5, 2.3, 0.8))

