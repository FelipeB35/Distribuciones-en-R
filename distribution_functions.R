library(ggplot2)
library(extraDistr)


get_theme_graph <- function(color="black"){
  theme_graphs =  theme(plot.title = element_text(hjust = 0.5), 
                        axis.title.x = element_text(face="bold", colour=color, size = 12),
                        axis.title.y = element_text(face="bold", colour=color, size = 12),
                        axis.title = element_text(face="bold", colour=color, size = 12))
  return(theme_graphs)
}

# Distribución uniforme
plot_uniform_distribution <- function(alpha=0, beta=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qgamma retorna valor de x dada un area de probabilidad
  lim_inf <-  alpha
  lim_sup <-  beta
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución uniform, alpha", alpha, ", beta", beta), collapse = " ")
  mean = (alpha +  beta) / 2
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dunif, 
      args=list(min = alpha, max = beta),
      geom="area", 
      color=color,
      fill = fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph() + 
    geom_vline(xintercept = mean, color="black", linetype="dotted")
}



# Distribución triangular
plot_triangular_distribution <- function(min=0, max=2, mode=1, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qtriang retorna valor de x dada un área de probabilidad
  lim_inf <-  qtriang(.01, a = min, b=max, c=mode)
  lim_sup <-  qtriang(.99, a = min, b=max, c=mode)
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución triangular"), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun = dtriang, 
      args=list(a = min, b=max, c=mode),
      geom = "area",
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph() +
    geom_vline(xintercept = (max + min + mode) /3, color=color, linetype="dotted")
}




# Definición de función normal
plot_normal_distribution <- function(mean = 0, sd = 1, fill="gray", color="black") {
  
  # Cálculo de los límites de x desde de la gráfica
  # usando teorema de Chebyshev: z = 4
  lim_inf <- -4 * sd + mean
  lim_sup <- 4 * sd + mean
  
  title = paste(c("Distribucion normal, media", mean, ", desviacion", sd), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf, lim_sup)) + # límites de x desde donde se grafica
    stat_function(
      fun=dnorm, args=list(mean=mean, sd=sd),
      color=color,
      fill = fill,
      geom="area"
      ) +
    get_theme_graph(color) + 
    labs(y="f(x)", x="x", title = title) +
    geom_vline(xintercept = mean, color=color, linetype="dotted")
}


# Distribución gamma
plot_gamma_distribution <- function(alpha=2, beta=2, fill="gray", color="black") {
  
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qgamma retorna valor de x dada un area de probabilidad
  lim_inf <-  qgamma(.01, shape = alpha, rate = (1 / beta))
  lim_sup <-  qgamma(.999, shape = alpha, rate = (1 / beta))
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribucion gamma, alpha", alpha, ", beta", beta), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dgamma, 
      args=list(shape = alpha, rate = (1 / beta)),
      geom="area", 
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph(color) +
    geom_vline(xintercept = alpha*beta, color=color, linetype="dotted")
}

# Distribución exponencial
plot_exponential_distribution <- function(beta=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qexp retorna valor de x dada un area de probabilidad
  lim_inf <-  qexp(.01, rate = (1 / beta))
  lim_sup <-  qexp(.999, rate = (1 / beta))
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución exponencial, beta", beta), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun = dexp, 
      args=list(rate = (1 / beta)),
      color=color,
      geom = "area", 
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph() +
    geom_vline(xintercept = beta, color=color, linetype="dotted")
}


# Distribución beta
plot_beta_distribution <- function(alpha=2, beta=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qbeta retorna valor de x dada un área de probabilidad
  lim_inf <-  qbeta(.01, shape1 = alpha, shape2 =  beta)
  lim_sup <-  qbeta(.99, shape1 = alpha, shape2 =  beta)
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución gamma, alpha", alpha, ", beta", beta), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dbeta, 
      args=list(shape1 = alpha, shape2 = beta),
      geom="area", 
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph() +
    geom_vline(xintercept = alpha/ (beta + alpha), color=color, linetype="dotted")
}



# Distribución de Weibull
plot_weibull_distribution <- function(alpha=1, beta=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qweibull retorna valor de x dada un área de probabilidad
  lim_inf <-  qweibull(.01, shape = beta, scale = alpha)
  lim_sup <-  qweibull(.99, shape = beta, scale = alpha)
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución weibull, alpha", alpha, ", beta", beta), collapse = " ")
  mean = alpha^(-1/beta) * gamma(1+(1/beta))
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dweibull, 
      args=list(shape = beta, scale = alpha),
      geom="area", 
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph() + 
    geom_vline(xintercept = mean, color="black", linetype="dotted")
}



#Distribucion LogNormal
plot_lognormal_distribution <- function(meanlog=2, sdlog=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qlnorm retorna valor de x dada un área de probabilidad
  lim_inf <-  qlnorm(.01, meanlog = meanlog, sdlog =  sdlog)
  lim_sup <-  qlnorm(.99, meanlog = meanlog, sdlog =  sdlog)
  
  
  
  offset <- (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución LogNormal"), collapse = " ")
  
  # Graficacion
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dlnorm, 
      args=list(meanlog = meanlog, sdlog = sdlog),
      geom="area", 
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title)  + get_theme_graph(fill=fill, color = color) +
    geom_vline(xintercept = meanlog, color=color, linetype="dotted")
}



# Distribución Erlang
plot_erlang_distribution <- function(r=2, lambda=2, fill="gray", color="black") {
  # Cálculo de los límites  de x de la gráfica
  # encontrando donde la distribución acumulada es 0.01 y 0.99
  # qgamma retorna valor de x dada un área de probabilidad
  lim_inf <-  qgamma(.01, shape = r, rate = (1 / lambda))
  lim_sup <-  qgamma(.999, shape = r, rate = (1 / lambda))
  
  offset = (lim_sup - lim_inf) / 4
  
  title = paste(c("Distribución gamma, alpha", alpha, ", beta", beta), collapse = " ")
  
  # Graficación
  ggplot() + 
    xlim(c(lim_inf - offset, lim_sup + offset)) + 
    stat_function(
      fun=dgamma, 
      args=list(shape = r, rate = (1 / lambda)),
      geom="area", 
      color=color,
      fill =fill) +
    labs(x = "\n x", y = "f(x) \n", 
         title = title) + 
    get_theme_graph(color) +
    geom_vline(xintercept = lambda*r, color=color, linetype="dotted")
}











