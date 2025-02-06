
source("dca.R") 

dca_results <- dca(
  data = dca_data,
  outcome = "CVD",
  predictors = predictors_list,
  graph = FALSE  
)

tiff(filename = "DCA_CVD_Comparison.tif", width = 600, height = 600, units = "px", 
     pointsize = 4, bg = "white", res = 300)

plot(
  dca_results$net.benefit$threshold, dca_results$net.benefit$none, 
  type = "l", lty = 2, lwd = 1, 
  xlim = c(0, 0.4), ylim = c(-0.05, 0.2), 
  xlab = "Threshold Probability", ylab = "Net Benefit",
  col = "black"
)

lines(dca_results$net.benefit$threshold, dca_results$net.benefit$all, lty = 1, col = "grey", lwd = 1)  # "All" strategy
lines(dca_results$net.benefit$threshold, dca_results$net.benefit$`Full Model`, lty = 1, col = "blue", lwd = 2)  # Full Model
lines(dca_results$net.benefit$threshold, dca_results$net.benefit$`ACES-only Model`, lty = 1, col = "red", lwd = 2)  # ACES-only Model

legend(
  "topright", 
  cex = 0.8, 
  legend = c("None", "All", "Full Model", "ACES-only Model"), 
  col = c("black", "grey", "blue", "red"), 
  lwd = c(1, 1, 2, 2), 
  lty = c(2, 1, 1, 1), 
  bty = "n"
)
dev.off()

# ==========================================================
# Script Completed: Decision Curve Analysis for CVD Models
# ==========================================================
