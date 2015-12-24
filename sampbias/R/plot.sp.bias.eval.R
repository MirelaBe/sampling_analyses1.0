plot.sp.bias.eval <- function(x, ...) {
  labs <- gsub("socioeconomic.","", rownames(x$details))
  
  plot(c(1, dim(x$details)[1]), 
       c(min(x$details$partial.B, na.rm = T),
         max(x$details$partial.B, na.rm = T)), 
       type = "n", ylab = "partial B", 
       xlab = "", axes = F)
  abline(h = 0, lty = 2)
  segments(seq(1, dim(x$details)[1]), rep(0, dim(x$details)[1]), 
           seq(1, dim(x$details)[1]), x$details$partial.B, lty = 2, 
           col = "grey80")
  axis(2)
  axis(1, labels = labs, at = seq(1, dim(x$details)[1]), 
       las = 2, cex.axis = 0.7)
  points(seq(1, dim(x$details)[1]), x$details$partial.B, col = "black", 
         pch = 16)
  box("plot")
} 