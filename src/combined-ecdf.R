combinedPlot <- function(.dim){
  colors_ <<- c(rep("black",2),rep("white",12))
  linetype <<- c(c(1:6),c(1:6),c(1:6))
  plotchar <<- c( 15,1,seq(16,15+8,1),8,13,14,13)
  layout(mat = matrix(1:16,nrow = 4,ncol = 4,byrow = TRUE),heights = c(1,1,1,1,0.2))
  par(oma=c(9, 6, 0, 6), mar=4*c(.1,.1,.1,.1), cex=1, las=1)
  ecdf_plot(.dim,1,3)
  ecdf_plot(.dim,4,10)
  ecdf_plot(.dim,11,14)
  ecdf_plot(.dim,16,20)
  ecdf_plot(.dim,21,28)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c('DES','CMA-ES'), text.font=2, cex=1.5, col=colors_[1:2],pch=plotchar[1:2], lty=linetype[1:2], ncol=2)

}
