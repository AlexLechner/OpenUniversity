#Linear regression example

#Create random data
set.seed(16)
NumberOfSamples <- 5
range<-.2
WaterPotential <- rep(0:5, each = 5)*.25*-1
GerminationProportionTemp <- (.52 * WaterPotential) +.7 #Using y = mx +c create linear data where m = .45 and c = .7
GerminationProportion <- runif(GerminationProportionTemp, GerminationProportionTemp-range,GerminationProportionTemp+range)
SimulatedData<-cbind(WaterPotential,GerminationProportion)


#Plot data
par(mar=c(4.5,4.1,1,1))
#sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.
ylim <- c(0,1)
xlim <- c(-1.5,0)
xaxis_label =" Water Potential MPa"
yaxis_label = "Germination Proportion"

#Histogram
hist(GerminationProportion, col="grey",xlab=yaxis_label, cex.lab=1.4, main =" ")

#R
cor(WaterPotential,GerminationProportion,  method = c("pearson"))

#Scatter plot of 
plot(SimulatedData, xlim = xlim, ylim = ylim, xlab=xaxis_label, ylab=yaxis_label, col = "dark red", cex.lab=1.4)

#Run regression
SimulatedData.lm = lm(GerminationProportion ~ WaterPotential, data = data.frame(SimulatedData))
abline(SimulatedData.lm, col = "blue")
print(summary(SimulatedData.lm))

#Residual analysis
SimulatedData.res = resid(SimulatedData.lm)
plot(SimulatedData[,1], SimulatedData.res, ylab="Residuals", xlab=xaxis_label, col = "dark red", cex.lab=1.4, xlim = xlim) 
abline(0, 0, col="blue")               


