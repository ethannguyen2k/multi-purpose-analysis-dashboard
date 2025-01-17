library(RCurl)
library(scales)

aromatase <- read.csv("https://raw.githubusercontent.com/dataprofessor/code/refs/heads/master/plot/scatter-plot/aromatase.csv", 
                      stringsAsFactors = T)

modified_aromatase <- cbind(
  class = aromatase[,2],
  aromatase[,6:18]
)

modified_aromatase$color <- factor(modified_aromatase$class,
                                   levels = c("Steroid", "Non-Steroid"),
                                   labels = c("royalblue", "red"))

plot(modified_aromatase$MW, modified_aromatase$ALogP)

plot(modified_aromatase$MW, modified_aromatase$ALogP, 
     pch = 20,
     col = as.character(modified_aromatase$color))

# R has 657 colors, colors() to list them all

plot(modified_aromatase$MW, modified_aromatase$ALogP, 
     pch = 20,
     col = rgb(0,0,0, alpha = 0.1))

# https://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r?title=r-plot-pch-symbols-the-different-point-shapes-available-in-r

plot(modified_aromatase$MW, modified_aromatase$ALogP, 
     pch = 20,
     col = alpha("royalblue", 0.3))

plot(modified_aromatase$MW, modified_aromatase$ALogP, 
     pch = 20,
     col = alpha(as.character(modified_aromatase$color), 0.3))

plot(modified_aromatase$MW, modified_aromatase$ALogP, 
     pch = 20,
     col = alpha(as.character(modified_aromatase$color), 0.3),
     xlab = "Molecular Weight (MW)",
     ylab = "Solubility (AlogP)",
     font.lab = 2
)

# Trendline
abline(lm(modified_aromatase$ALogP ~ modified_aromatase$MW))

# Creating multi-plot figures

par(mfrow=c(2,2), mai=c(.7,.7,.3,.3))
  plot(modified_aromatase$MW, modified_aromatase$ALogP)
  plot(modified_aromatase$MW, modified_aromatase$Qm)
  plot(modified_aromatase$HOMO, modified_aromatase$LUMO)
  plot(modified_aromatase$MW, modified_aromatase$HOMO)

par(mfrow=c(3,1), mai=c(.3,.7,.1,.3))
  plot(modified_aromatase$MW, modified_aromatase$ALogP)
  plot(modified_aromatase$MW, modified_aromatase$Qm)
  plot(modified_aromatase$MW, modified_aromatase$HOMO)

# Saving plot
pdf("plot_multiplot.pdf")
  par(mfrow=c(1,3),  mai = c(0.3, 0.3, 0.3, 0))
  plot(modified_aromatase$MW, modified_aromatase$ALogP)
  plot(modified_aromatase$MW, modified_aromatase$Qm)
  plot(modified_aromatase$MW, modified_aromatase$HOMO)
dev.off()