## funçoes
#distribuicao acumulada

F.NME_weibull <- function(x, alpha, delta){
  phi_1 = delta[1]
  phi_2 = delta[2]
  
  aux = (exp(-phi_2*x^phi_1))
  p1 <- (alpha^2)*aux
  p2 <- (alpha + (1 - aux))^2
  
  eq = 1 - p1/p2
  return(eq)
}

#funcao de densidade
d.NME_weibull <- function(theta, x, log = F){
  alpha = theta[1]
  phi_1 = theta[2]
  phi_2 = theta[3]
  
  aux = (exp(-phi_2*(x^phi_1)))
  
  p1 <- (alpha^2)*phi_1*phi_2*x^(phi_1-1)*aux
  p2 <- (alpha + 1 -aux)^3
  p3 <- alpha + 2 -(1-aux)
  
  eq = (p1/p2)*(p3)
  if(log == T){
    return(log(eq))
  }else{
    return(eq)  
  }
  
}
# equacao_6.2
#funcao de risco 
h.NME_weibull <- function(theta, x){
  alpha = theta[1]
  phi_1 = theta[2]
  phi_2 = theta[3]
  
  aux = (exp(-phi_2*x^(phi_1)))
  p1 <- phi_1*phi_2*x^(phi_1-1)
  p2 <- (alpha + 1 -aux)
  p3 <- alpha + 2 -(1-aux)
  
  eq = (p1/p2)*(p3)
}
# equacao_6.3
# Funcao de risco acumulada
H.NME_weibull <- function(x, alpha, delta){
  phi_1 = delta[1]
  phi_2 = delta[2]
  
  aux = (exp(-phi_2*x^(phi_1)))
  
  p1 <- (alpha^2)*aux
  p2 <- (alpha + 1- aux)^2 
  
  eq = -log((p1/p2))
  
}

#equacao 6.1 - funçao de sobrevivencia

S.NME_weibull <- function(x, alpha, delta){
  phi_1 = delta[1]
  phi_2 = delta[2]
  
  aux = (exp(-phi_2*x^phi_1))
  p1 <- (alpha^2)*aux
  p2 <- (alpha + (1 - aux))^2
  
  eq = 1- p1/p2
}

#varlores aleatórios da distribuicao NME_weibull
R.NME_Weibull <- function(theta, n){
  alpha = theta[1]
  phi_1 = theta[2]
  phi_2 = theta[3]
  
  u = runif(n, min = 0, max = 1)
  aux1 <- alpha + 1
  aux2 <- alpha + 2
  aux3 <- alpha*sqrt(aux2^2 - 4*aux1*u)
  
  p1 <- 2*aux1*u - alpha*aux2 -2 -aux3
  p2 <- 2*(aux1^2)*(u-1)
  
  q = ((1/phi_2)*log(p1/p2))^(1/phi_1)
  return(q)
  

}

#l_verossimilhanca

l_vero <- function(theta, x){
  l = sum(d.NME_weibull(x = x, theta=theta, log = T))
  return(-l)
}


#simulacao bootstrap


sim.boot<-function(amostra,B, theta,theta0,alpha_IC){
  
  alpha.np <-  numeric()
  phi_1.np <-  numeric()
  phi_2.np <-  numeric()
  alpha.p <- numeric()
  phi_1.p <- numeric()
  phi_2.p <- numeric()
  
  
  op <- optim(par=theta0,fn=l_vero,x=amostra,method = "L-BFGS-B",
              lower = c(0.01,0.01,0.01), upper = c(Inf, Inf, Inf),
              hessian = TRUE)
  
  for(b in 1:B) {
    
    #Nao parametrico
    amostra.np <- sample(amostra,size=length(amostra),replace = TRUE)
    op.np = optim(par=theta0,fn=l_vero,x=amostra.np,method = "L-BFGS-B",
                  lower = c(0.01,0.01,0.01), upper = c(Inf, Inf, Inf),
                  hessian = TRUE)
    #valores dos parametos de acordo com a funcao optim
    
    alpha.np[b] <- op.np$par[1]
    phi_1.np[b] <- op.np$par[2]
    phi_2.np[b] <- op.np$par[3]
    
    #Parametrico
    amostra.p <- R.NME_Weibull(n=length(amostra),
                               theta = c(op$par[1], op$par[2], op$par[3]))
    
    op.p = optim(par=theta0,fn=l_vero,x=amostra.p,method = "L-BFGS-B",
                 lower = c(0.01,0.01,0.01), upper = c(Inf, Inf, Inf),
                 hessian = TRUE)
    
    alpha.p[b] <- op.p$par[1]
    phi_1.p[b] <- op.p$par[2]
    phi_2.p[b] <- op.p$par[3]
    
  }
  
  #vies
  vies.np_alpha <- alpha.np - mean(alpha.np)
  vies.np_phi_1 <- phi_1.np - mean(phi_1.np)
  vies.np_phi_2 <- phi_2.np - mean(phi_2.np)
  vies.p_alpha <- alpha.p - mean(alpha.p)
  vies.p_phi_1 <- phi_1.p - mean(phi_1.p)
  vies.p_phi_2 <- phi_2.p - mean(phi_2.p)
  #erro padrao
  ep.np_alpha <- sqrt(sum(vies.np_alpha^2)/(length(amostra)-1))
  ep.np_phi_1 <- sqrt(sum(vies.np_phi_1^2)/(length(amostra)-1))
  ep.np_phi_2 <- sqrt(sum(vies.np_phi_2^2)/(length(amostra)-1))
  ep.p_alpha <- sqrt(sum(vies.p_alpha ^2)/(length(amostra)-1))
  ep.p_phi_1 <- sqrt(sum(vies.p_phi_1 ^2)/(length(amostra)-1))
  ep.p_phi_2 <- sqrt(sum(vies.p_phi_2 ^2)/(length(amostra)-1))
  
  #armazenamento das informacoes
  resultado<-list()
  resultado$descricao <- paste("Boostrat com n = ", length(amostra),
                               " e B = ",  B, "alpha = ", theta[1], 
                               "; phi_1 = ", theta[2]  ,"; phi_2 = ", theta[3],"." )
  
  resultado$MA_thetas.np <- data.frame(alpha.np,phi_1.np,phi_2.np)
  resultado$MA_thetas.p <- data.frame(alpha.p,phi_1.p,phi_2.p)
  resultado$MA_vies.np <-data.frame(vies.np_alpha,vies.np_phi_1, vies.np_phi_2)
  resultado$MA_vies.p <-data.frame(vies.p_alpha,vies.p_phi_1, vies.p_phi_2)
  resultado$MA_ep.np <-data.frame(ep.np_alpha,ep.np_phi_1, ep.np_phi_2)
  resultado$MA_ep.p <-data.frame(ep.p_alpha,ep.p_phi_1, ep.p_phi_2)
    
    
  resultado$Estimativa.boot <- c(mean(alpha.np),
                                 mean(phi_1.np),
                                 mean(phi_2.np),
                                 mean(alpha.p),
                                 mean(phi_1.p),
                                 mean(phi_2.p))
  
  resultado$Vies.boot <- c(mean(vies.np_alpha),
                           mean(vies.np_phi_1),
                           mean(vies.np_phi_2),
                           mean(vies.p_alpha),
                           mean(vies.p_phi_1),
                           mean(vies.p_phi_2))
  
  resultado$ep.boot <- c(ep.np_alpha,
                         ep.np_phi_1,
                         ep.np_phi_2,
                         ep.p_alpha,
                         ep.p_phi_1,
                         ep.p_phi_2 )
  
  resultado$IC.boot<-rbind(quantile(alpha.np,c(alpha_IC/2,1 - alpha_IC/2)),
                           quantile(phi_1.np,c(alpha_IC/2,1 - alpha_IC/2)),
                           quantile(phi_2.np,c(alpha_IC/2,1 - alpha_IC/2)),
                           quantile(alpha.p,c(alpha_IC/2,1 - alpha_IC/2)),
                           quantile(phi_1.p,c(alpha_IC/2,1 - alpha_IC/2)),
                           quantile(phi_2.p,c(alpha_IC/2,1 - alpha_IC/2)))
  return(resultado)
}



#generalizando para outras possibilidades de thetas, thetas iniciais, tamanho da amostra 

#sim_boot_g <- function(theta, theta0,alpha_IC, n_B, amostra){
#  
#  amostraB <- sim.boot(amostra = amostra, B = n_B, 
#                       theta0 = theta0, alpha_IC = alpha_IC)
#  amostraB$descricao<- paste0("Bootstrap com n= ", length(amostra),
#                             " e B = ", n_B, "alpha = ", theta[1], 
#                             "phi_1 = ", theta[2]  ,"phi_2 = ", theta[3] )
#  return(amostraB)
#}



