data = read.csv(file="data_titret.csv", header=TRUE, sep=";")

v_naoh = data$v_naoh
ph_hcl = data$ph_hcl
ph_as = data$ph_as
v_hcl = v_naoh[1:49]
ph_hcl = ph_hcl[1:49]

print(ph_hcl)

phCounter = seq(1.8, 12, 0.2)
# print(phCounter)
phVolumeHCl = c()

for (e in ph_hcl){
	for (i in phCounter){
		if (e == i){
			index = which(e == ph_hcl)[[1]]
			# print(e)
			# print(v_hcl[index])
			phVolumeHCl = c(phVolumeHCl, v_hcl[index])
		}
		else if (e - 0.13 < i){
			if (e + 0.13 > i){
				index = which(e == ph_hcl)[[1]]
				phVolumeHCl = c(phVolumeHCl, v_hcl[index])
			}
		}
	}	
}
print(length(phCounter))
print(length(phVolumeHCl))
# test = data.frame(phCounter, phVolumeHCl)
# print(test)

# data_means = data.frame(ph_hcl, v_hcl)
# print(data_means)
# print((1.97+2.11)/2)

# 1.97 - 1800
# 2.00 - x
# x = 2.00 * 1800 / 1.97

# x = 2.8 * 2100 / 2.7
# print(x)

##################################
# In R, I have an element x and a vector v.
# I want to find the first index of an element in v that is equal to x.
# which(x == v)[[1]]
##################################
hcl.spl = smooth.spline(v_hcl, ph_hcl)
as.spl = smooth.spline(v_naoh, ph_as)

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