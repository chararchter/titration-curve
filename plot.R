data = read.csv(file="data_titret.csv", header=TRUE, sep=";")

v_naoh = data$v_naoh
ph_hcl = data$ph_hcl
ph_as = data$ph_as
v_hcl = v_naoh[1:49]
ph_hcl = ph_hcl[1:49]
phCounter = seq(1.8, 12, 0.2)
phVolumeHCl = c()
phVolumeAS = c()
#############################################
# isEqual = function(e,i){
# 	if (e == i){
# 		index = which(e == ph_hcl)[[1]]
# 		phVolumeHCl = c(phVolumeHCl, v_hcl[index])
# 	}
# 	return(phVolumeHCl)
# }
# for (e in ph_hcl){
# 	for (i in phCounter){
# 		isEqual(e,i)		
# 			if (e - 0.13 < i){
# 			if (e + 0.13 > i){
# 				index = which(e == ph_hcl)[[1]]
# 				phVolumeHCl = c(phVolumeHCl, v_hcl[index])
# 			}
# 		}
# 	}	
# }
#test if two vectors have the same weights
# print(length(phCounter))
# print(length(phVolumeHCl))
#############################################

hcl.spl = smooth.spline(v_hcl, ph_hcl)
as.spl = smooth.spline(v_naoh, ph_as)

#binārā meklēšana
spl = function(val, e, spline) {
	x_min = 0
	x_max = 5000
	while(TRUE) {
		x_avg = (x_min + x_max) / 2
		pred = predict(spline, x_avg)
		y = pred$y
		if(y < val - e) {
			x_min = x_avg
		} else if(y > val + e) {
			x_max = x_avg
		} else {
			return(x_avg)
		}
	}
}

spl(2, 0.001, hcl.spl)

for (y in phCounter){
	precision = 0.001
	phi = round(spl(y, precision, hcl.spl), digits = 0)
	phVolumeHCl = c(phVolumeHCl, phi)
	asi = round(spl(y, precision, as.spl), digits = 0)
	phVolumeAS = c(phVolumeAS, asi)
}

corVolume = phVolumeAS - phVolumeHCl
# print(corVolume)
# print(phVolumeHCl)
# print(phVolumeAS)
test = data.frame(phCounter, phVolumeHCl, phVolumeAS, corVolume)
print(test)

plot.new()
jpeg('ph_hcl.jpeg', width = 900, height = 500, units = "px", pointsize = 10)
plot(v_hcl, ph_hcl, pch=20, xlim =c(0,3500), col = "gray25", xlab = expression(paste("NaOH šķīduma tilpums, ", mu, "l")),
	ylab="pH", cex.axis = 1.5, cex.lab=1.5)
lines(hcl.spl, col = "blue")
title(main = 'Alanīna titrēšanas līkne', cex.main = 2, font.main= 4, col.main= "black")
abline(v=(seq(0,3500,100)), col="burlywood4", lty="dotted")
abline(v=(seq(0,3500,50)), col="burlywood2", lty="dotted")
abline(h=(seq(0,12,1)), col="burlywood4", lty="dotted")
abline(h=(seq(0,12,0.5)), col="burlywood2", lty="dotted")
dev.off()

plot.new()
jpeg('ph_as.jpeg', width = 900, height = 500, units = "px", pointsize = 10)
plot(v_naoh, ph_as, pch=20, col = "gray25", xlab = expression(paste("NaOH šķīduma tilpums, ", mu, "l")),
	ylab="pH", cex.axis = 1.5, cex.lab=1.5)
lines(as.spl, col = "blue")
title(main = 'Alanīna titrēšanas līkne', cex.main = 2, font.main= 4, col.main= "black")
abline(v=(seq(0,5500,100)), col="burlywood4", lty="dotted")
abline(v=(seq(0,5500,50)), col="burlywood2", lty="dotted")
abline(h=(seq(0,12,1)), col="burlywood4", lty="dotted")
abline(h=(seq(0,12,0.5)), col="burlywood2", lty="dotted")
dev.off()