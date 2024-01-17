

# graficos#### 

#cenario (a) alpha = 0.5, phi_1 = 1.0, phi_2 = 0.8
theta_a = c(0.5, 1.0,0.8)
theta_b = c(0.8, 1.0,1.2)
theta_c = c(1.4, 1.0,0.4)

set.seed(1234)
amostra_ca <- R.NME_Weibull(theta = theta_a, n = 1000)
#
#hist(amostra_ca, freq=F, breaks = 100,
#     main = "Histograma de números gerados",
#     xlab = "Valores da amostra 2")

#funcao de densidade
setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\figuras")
jpeg(paste0("fx.png"), quality = 100, width = 1400, height = 655)
par(mfrow = c(1,2))
curve(d.NME_weibull(x,theta = theta_a),
      from = 0, to =max(amostra_ca), col = "red", ylim = range(0:4.5),
      lwd = 3.5,# yaxt = "n",
      # ylab = expression("f "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = ""
)

curve(d.NME_weibull(x,theta = theta_b),
      from = 0, to =max(amostra_ca), col = "blue",
      lwd = 1.5, add = TRUE, #yaxt = "n",
      # ylab = expression("f "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = ""
)

curve(d.NME_weibull(x,theta = theta_c),
      from = 0, to =max(amostra_ca), col = "orange",cex = 2,
      lwd = 2, add = TRUE, #yaxt = "n",
      #ylab = expression("f "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = ""
)
mtext(expression("f "(x~";"~alpha~","~Lambda)),side=2, line=2, col="black", cex=1.2)
mtext("x", side=1, line=2, col="black", cex=1.5)

legend(x = 4, y =4, box.lty=0,  #lty = c(3,7),
       text.font = 1, cex = 1,
       fill= c("red","blue", "orange"),#text.col = "black", 
       legend=c(expression(alpha == 0.5 ~','~phi[1] == 1.0 ~","~phi[2] == 0.8),
                expression(alpha == 0.8 ~','~phi[1] == 1.0 ~","~phi[2] == 1.2),
                expression(alpha == 1.4 ~','~phi[1] == 1.0 ~","~phi[2] == 0.4)))
dev.off()
#funcao de risco
jpeg(paste0("hx.png"), quality = 100, width = 700, height = 655)

curve(h.NME_weibull(x,theta = theta_a),
      from = 0, to =max(amostra_ca), col = "red", ylim = range(0:4.5),
      lwd = 2,  #yaxt = "n",
      # ylab = expression("h "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = "")

curve(h.NME_weibull(x,theta = theta_b),
      from = 0, to =max(amostra_ca), col = "blue",
      lwd = 2, add = TRUE, #yaxt = "n",
      # ylab = expression("h "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = "")

curve(h.NME_weibull(x,theta = theta_c),
      from = 0, to =max(amostra_ca), col = "orange",cex = 2,
      lwd = 2, add = TRUE,# yaxt = "n",
      # ylab = expression("h "(x~";"~alpha~","~Lambda))
      ylab = "", xlab = "")

legend(x = 4, y =4,box.lty=0, #lty = c(3,7),
       text.font = 1, cex = 1,
       fill= c("red","blue", "orange"),#text.col = "black", 
       legend=c(expression(alpha == 0.5 ~','~phi[1] == 1.0 ~","~phi[2] == 0.8),
                expression(alpha == 0.8 ~','~phi[1] == 1.0 ~","~phi[2] == 1.2),
                expression(alpha == 1.4 ~','~phi[1] == 1.0 ~","~phi[2] == 0.4)))
mtext(expression("h "(x~";"~alpha~","~Lambda)),side=2, line=2, col="black", cex=1.2)
mtext("x", side=1, line=2, col="black", cex=1.5)

dev.off()
##bootstrap
n_sample = c(100,250, 400,500)
n_B = c(100, 500, 700)

theta_a = c(0.5, 1.0,0.8)
theta0_a = c(0.4,0.8,0.7)
alpha_IC = 0.05

data.frame(matrix( c(0.5, 1.0,0.8,
                     0.4,0.8,0.7), ncol = 2), names = c('a', "b", "c"))

#amostras ####

set.seed(123)
amostra_ca_100 <- R.NME_Weibull(theta = theta_a, n = n_sample[1])
set.seed(12344)
amostra_ca_250 <- R.NME_Weibull(theta = theta_a, n = n_sample[2])
set.seed(123445)
amostra_ca_400 <- R.NME_Weibull(theta = theta_a, n = n_sample[3])
set.seed(123564)
amostra_ca_500 <- R.NME_Weibull(theta = theta_a, n = n_sample[4])

set.seed(134)
amostra_cc_100 <- R.NME_Weibull(theta = theta_c, n = n_sample[1])
set.seed(12234)
amostra_cc_250 <- R.NME_Weibull(theta = theta_c, n = n_sample[2])
set.seed(15234)
amostra_cc_400 <- R.NME_Weibull(theta = theta_c, n = n_sample[3])
set.seed(15834)
amostra_cc_500 <- R.NME_Weibull(theta = theta_c, n = n_sample[4])


hist(amostra_ca_100)
hist(amostra_ca_250 )
hist(amostra_ca_400)

hist(amostra_cc_100)
hist(amostra_cc_250)
hist(amostra_cc_400)

###Boots amostras cenario  a####  

set.seed(543);b_ca_100_100 <- sim.boot(amostra_ca_100, B = n_B[1], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(544);b_ca_100_500 <- sim.boot(amostra_ca_100, B = n_B[2], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5546);b_ca_100_700 <- sim.boot(amostra_ca_100, B = n_B[3], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(546);b_ca_250_100 <- sim.boot(amostra_ca_250 , B = n_B[1], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(547);b_ca_250_500 <- sim.boot(amostra_ca_250 , B = n_B[2], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(548);b_ca_250_700 <- sim.boot(amostra_ca_250 , B = n_B[3], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5449);b_ca_400_100 <- sim.boot(amostra_ca_400, B = n_B[1], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5540);b_ca_400_500 <- sim.boot(amostra_ca_400, B = n_B[2], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5451);b_ca_400_700 <- sim.boot(amostra_ca_400, B = n_B[3], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5479);b_ca_500_100 <- sim.boot(amostra_ca_500, B = n_B[1], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(55710);b_ca_500_500 <- sim.boot(amostra_ca_500, B = n_B[2], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)
set.seed(5571);b_ca_500_700 <- sim.boot(amostra_ca_500, B = n_B[3], theta = theta_a, theta0 = theta0_a, alpha_IC = alpha_IC)


###Boots amostras cenario  c#### 
theta_c = c(1.4, 1.0,0.4)
theta0_c = c(1.2,0.8,0.3)


set.seed(12380);b_cc_100_100 <- sim.boot(amostra_cc_100, B = n_B[1], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12381);b_cc_100_500 <- sim.boot(amostra_cc_100, B = n_B[2], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(13382);b_cc_100_700 <- sim.boot(amostra_cc_100, B = n_B[3], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12383);b_cc_250_100 <- sim.boot(amostra_cc_250, B = n_B[1], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12385);b_cc_250_500 <- sim.boot(amostra_cc_250, B = n_B[2], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12387);b_cc_250_700 <- sim.boot(amostra_cc_250, B = n_B[3], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12388);b_cc_400_100 <- sim.boot(amostra_cc_400, B = n_B[1], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(1289);b_cc_400_500 <- sim.boot(amostra_cc_400, B = n_B[2], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(1390);b_cc_400_700 <- sim.boot(amostra_cc_400, B = n_B[3], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12398);b_cc_500_100 <- sim.boot(amostra_cc_500, B = n_B[1], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(12789);b_cc_500_500 <- sim.boot(amostra_cc_500, B = n_B[2], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)
set.seed(17390);b_cc_500_700 <- sim.boot(amostra_cc_500, B = n_B[3], theta = theta_c, theta0 = theta0_c, alpha_IC = alpha_IC)

#graficos ####

###cenário a#### 
####Parte não paramétrica####


setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\figuras")
jpeg(paste0("ca_alpha_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_ca_100_100$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(alpha)))#alpha
plot(b_ca_100_500$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_100_700$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_100$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_500$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_500$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_100$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_500$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_700$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_100$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_500$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_700$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(alpha)))

dev.off()

jpeg(paste0("ca_phi_1_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_ca_100_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[1])))#phi_1
plot(b_ca_100_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_100_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[1])))

dev.off()


jpeg(paste0("ca_phi_2_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_ca_100_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[2])))#phi_2
plot(b_ca_100_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_100_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
dev.off()


#### Parte paramétrica####
jpeg(paste0("ca_alpha_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_ca_100_100$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(alpha)))#alpha
plot(b_ca_100_500$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_100_700$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_100$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_500$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_250_500$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_100$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_500$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_400_700$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_100$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_500$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_ca_500_700$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(alpha)))

dev.off()

jpeg(paste0("ca_phi_1_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_ca_100_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[1])))#phi_1
plot(b_ca_100_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_100_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_250_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_400_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_ca_500_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[1])))

dev.off()

jpeg(paste0("ca_phi_2_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))

plot(b_ca_100_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[2])))#phi_2
plot(b_ca_100_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_100_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_250_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_400_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_ca_500_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[2])))

dev.off()

##cenario c####

####Parte não paramétrica####


setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\figuras")
jpeg(paste0("cc_alpha_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_cc_100_100$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(alpha)))#alpha
plot(b_cc_100_500$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_100_700$MA_thetas.np$alpha.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_100$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_500$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_700$MA_thetas.np$alpha.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_100$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_500$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_700$MA_thetas.np$alpha.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_100$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_500$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_700$MA_thetas.np$alpha.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(alpha)))

dev.off()

jpeg(paste0("cc_phi_1_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_cc_100_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[1])))#phi_1
plot(b_cc_100_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_100_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_100$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_500$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_700$MA_thetas.np$phi_1.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[1])))

dev.off()

jpeg(paste0("cc_phi_2_np.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_cc_100_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[2])))#phi_2
plot(b_cc_100_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_100_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_100$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_500$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_700$MA_thetas.np$phi_2.np, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[2])))

dev.off()

####Parte paramétrica####
jpeg(paste0("cc_alpha_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_cc_100_100$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(alpha)))#alpha
plot(b_cc_100_500$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_100_700$MA_thetas.p$alpha.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_100$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_500$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_250_700$MA_thetas.p$alpha.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_100$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_500$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_400_700$MA_thetas.p$alpha.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_100$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_500$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(alpha)))
plot(b_cc_500_700$MA_thetas.p$alpha.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(alpha)))

dev.off()

jpeg(paste0("cc_phi_1_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))
plot(b_cc_100_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[1])))#phi_1
plot(b_cc_100_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_100_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_250_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_400_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_100$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_500$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[1])))
plot(b_cc_500_700$MA_thetas.p$phi_1.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[1])))

dev.off()

jpeg(paste0("cc_phi_2_p.png"), quality = 100, width = 662, height = 583)
par(mfrow = c(4,3))

plot(b_cc_100_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 100", xlab = "n", ylab = expression(hat(phi[2])))#phi_2
plot(b_cc_100_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_100_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 100, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_250_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 250, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_400_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 400, B = 700", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_100$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 100", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_500$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 500", xlab = "n", ylab = expression(hat(phi[2])))
plot(b_cc_500_700$MA_thetas.p$phi_2.p, type = "l", main = "n = 500, B = 700", xlab = "n", ylab = expression(hat(phi[2])))

dev.off()

#Tabelas####

setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\tabelas")

##estimativa boots####

###cenario a####

tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_100_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_100_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_ca_100_700$Estimativa.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_250_100$Estimativa.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_500$Estimativa.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_700$Estimativa.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_400_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_700$Estimativa.boot, ncol = 2)))

tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_500_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_700$Estimativa.boot, ncol = 2)))

tab.estboots_ca_final <- rbind(tab_n100_np_p,
                               tab_n250_np_p,
                               tab_n400_np_p,
                               tab_n500_np_p)

names(tab.estboots_ca_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)



###cenario c####


tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_100_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_100_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_cc_100_700$Estimativa.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_250_100$Estimativa.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_500$Estimativa.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_700$Estimativa.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_400_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_700$Estimativa.boot, ncol = 2)))

tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_500_100$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_500$Estimativa.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_700$Estimativa.boot, ncol = 2)))

tab.estboots_cc_final <- rbind(tab_n100_np_p,
                               tab_n250_np_p,
                               tab_n400_np_p,
                               tab_n500_np_p)

names(tab.estboots_cc_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)

setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\tabelas")
write.csv2(tab.estboots_ca_final, "est.boots.tab_ca_final.csv")

write.csv2(tab.estboots_cc_final, "est.boots.tab_cc_final.csv")

##vies####
###cenario a####
tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_100_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_100_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_ca_100_700$Vies.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_250_100$Vies.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_500$Vies.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_500$Vies.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_400_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_700$Vies.boot, ncol = 2)))

tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_500_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_700$Vies.boot, ncol = 2)))

tab.vies_ca_final <- rbind(tab_n100_np_p,
                           tab_n250_np_p,
                           tab_n400_np_p,
                           tab_n500_np_p)

names(tab.vies_ca_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)



###cenario c####


tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_100_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_100_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_cc_100_700$Vies.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_250_100$Vies.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_500$Vies.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_700$Vies.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_400_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_700$Vies.boot, ncol = 2)))

tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_500_100$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_500$Vies.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_700$Vies.boot, ncol = 2)))


tab.vies_cc_final <- rbind(tab_n100_np_p,
                           tab_n250_np_p,
                           tab_n400_np_p)

names(tab.vies_cc_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)

setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\tabelas")
write.csv2(tab.vies_ca_final, "vies.bots.tab_ca_final.csv")

write.csv2(tab.vies_cc_final, "vies.bots.tab_cc_final.csv")

##erro padrao####
####cenario a####
tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_100_100$ep.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_100_500$ep.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_ca_100_700$ep.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_250_100$ep.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_500$ep.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_ca_250_500$ep.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_400_100$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_500$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_400_700$ep.boot, ncol = 2)))


tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_ca_500_100$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_500$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_ca_500_700$ep.boot, ncol = 2)))

tab.ep_ca_final <- rbind(tab_n100_np_p,
                         tab_n250_np_p,
                         tab_n400_np_p,
                         tab_n500_np_p)

names(tab.ep_ca_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)



###cenario c####


tab_n100_np_p<- cbind(data.frame(n= "100",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_100_100$ep.boot, ncol = 2)),
                      data.frame(#n= "100",B = "500", par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_100_500$ep.boot, ncol = 2)),
                      data.frame(#n= "100",B = "700",par = c("alpha", "phi_1", "phi_2"),
                        matrix(b_cc_100_700$ep.boot, ncol = 2)))

tab_n250_np_p <-  cbind(data.frame(n= "250",#B = "100",
                                   par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_250_100$ep.boot, ncol = 2)),
                        data.frame(#n= "250",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_500$ep.boot, ncol = 2)),
                        data.frame(#n= "250",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                          matrix(b_cc_250_700$ep.boot, ncol = 2)))


tab_n400_np_p<- cbind(data.frame(n= "400",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_400_100$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_500$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_400_700$ep.boot, ncol = 2)))

tab_n500_np_p<- cbind(data.frame(n= "500",#B = "100",
                                 par = c("alpha", "phi_1", "phi_2"), matrix(b_cc_500_100$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "500",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_500$ep.boot, ncol = 2)),
                      data.frame(#n= "400",B = "700",par = c("alpha", "phi_1", "phi_2"), 
                        matrix(b_cc_500_700$ep.boot, ncol = 2)))

tab.ep_cc_final <- rbind(tab_n100_np_p,
                         tab_n250_np_p,
                         tab_n400_np_p,
                         tab_n500_np_p)
names(tab.ep_cc_final) <- c("n", "par", "B_100_np", "B_100_p", "B_500_np", "B_500_p", "B_700_np", "B_700_p")
rm(tab_n100_np_p,tab_n250_np_p, tab_n400_np_p)

setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\tabelas")
write.csv2(tab.ep_ca_final, "ep.bots.tab_ca_final.csv")

write.csv2(tab.ep_cc_final, "ep.bots.tab_cc_final.csv")


setwd("S:\\dipeq\\copesp\\1 - Pastas pessoais\\Larissa\\artigo_computacional\\imagem.R")

save.image("artigo_imagem_22122023.RData")

load("artigo_imagem_20122023.RData")
