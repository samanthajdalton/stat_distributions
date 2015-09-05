library(RColorBrewer)
library("ggplot2")
library("reshape2")

#================================================================================
#    Bernouli distributions
#================================================================================

bernoulli <- function(p) {
    
    outcome <- data.frame(x = c(0, 1), y = c(p, 1 -p))
    
    ggplot(data = outcome, aes(x = as.factor(x), y = y)) +
        geom_point(size = 8) + ylim(0,1) +
        xlab("event") + ylab("probability") + theme_bw() +
        geom_linerange(aes(ymax=y,ymin=0),size=1) +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"))
    
}



#================================================================================
#    Binomial distributions
#================================================================================

binomial.dynamic <- function(n, p) {
    
    x.data <- data.frame(x = seq(0, n), p = dbinom(seq(0, n), n, p))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" n =", n, ", p =", p, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(color = "#ba064e", linetype="dotted") +
        #geom_line(data = x.data1, aes(x = x, y = p)) +
        geom_point(size = 4, aes(color = "P")) +
        xlab("x") + ylab("probability") +
        geom_linerange(aes(ymax=p,ymin=0),size= 0.7, color = "#ba064e") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical")
    
}


binomial.static <- function() {
    
    cols <- c("P1" = "#2f4074", "P2" = "#4bb74c", "P3" = "#ba064e") 
    
    x.data1 <- data.frame(x = seq(0, 20), p = dbinom(seq(0, 20), 20, 0.5))
    x.data2 <- data.frame(x = seq(0, 20), p = dbinom(seq(0, 20), 20, 0.7))
    x.data3 <- data.frame(x = seq(0, 40), p = dbinom(seq(0, 40), 40, 0.5))
    
    ggplot() +
        geom_point(data = x.data1, aes(x = x, y = p, color = "P1"), size = 4) +
        geom_point(data = x.data2, aes(x = x, y = p, color = "P2"), size = 4) +
        geom_point(data = x.data3, aes(x = x, y = p, color = "P3"), size = 4) +
        geom_line(data = x.data1, aes(x = x, y = p), linetype="dotted", color = "#2f4074") +
        geom_line(data = x.data2, aes(x = x, y = p), linetype="dotted", color = "#4bb74c") +
        geom_line(data = x.data3, aes(x = x, y = p), linetype="dotted", color = "#ba064e") +
        xlab("x") + ylab("probability") +
        scale_colour_manual(name="",values=cols, labels = c(" n = 20, p = 0.5", " n = 20, p = 0.7", " n = 40, p = 0.5")) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical")
    
}

#================================================================================
#    Poisson distributions
#================================================================================

poisson.dynamic <- function(k, lambda) {
    
    x.data <- data.frame(x = seq(0, k), p = dpois(seq(0, k), lambda))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" k =", k, ", lamba =", lambda, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(color = "#ba064e", linetype="dotted") +
        geom_point(size = 4, aes(color = "P")) +
        xlab("x") + ylab("probability") +
        geom_linerange(aes(ymax=p,ymin=0),size= 0.7, color = "#ba064e") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical")
    
}


poisson.static <- function() {
    
    cols <- c("P1" = "#7b3294", "P2" = "#abd9e9", "P3" = "#e66101") 
    
    x.data1 <- data.frame(x = seq(0, 20), p = dpois(seq(0, 20), 1))
    x.data2 <- data.frame(x = seq(0, 20), p = dpois(seq(0, 20), 4))
    x.data3 <- data.frame(x = seq(0, 20), p = dpois(seq(0, 20), 10))
    
    ggplot() +
        geom_point(data = x.data1, aes(x = x, y = p, color = "P1"), size = 4) +
        geom_point(data = x.data2, aes(x = x, y = p, color = "P2"), size = 4) +
        geom_point(data = x.data3, aes(x = x, y = p, color = "P3"), size = 4) +
        geom_line(data = x.data1, aes(x = x, y = p), linetype="dotted", color = "#2f4074") +
        geom_line(data = x.data2, aes(x = x, y = p), linetype="dotted", color = "#4bb74c") +
        geom_line(data = x.data3, aes(x = x, y = p), linetype="dotted", color = "#ba064e") +
        xlab("x") + ylab("probability") +
        scale_colour_manual(name="",values=cols, labels = c(" k = 20, lambda = 1", " k = 20, lambda = 4", " k = 40, lambda = 10")) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical")
    
}


#================================================================================
#    Uniform distributions
#================================================================================
uniform.dynamic <- function(a, b) {
    
    x.data <- data.frame(x = seq((a - 1), (b + 1), 0.01), p = dunif(seq((a - 1), (b + 1), 0.01), a, b))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" a =", a, ", b =", b, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(aes(color = "P"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
              scale_x_continuous(minor_breaks = seq((a - 1), (b + 1), 1), breaks = seq((a - 1), (b + 1), 1))
    
}


uniform.static <- function() {
    
    cols <- c("P1" = "#7b3294", "P2" = "#abd9e9", "P3" = "#e66101") 
    
    x.data1 <- data.frame(x = seq(-3,  5, 0.01), p = dunif(seq(-3,  5, 0.01), 0, 2))
    x.data2 <- data.frame(x = seq(-3,  5, 0.01), p = dunif(seq(-3,  5, 0.01), -2, 3))
    x.data3 <- data.frame(x = seq(-3,  5, 0.01), p = dunif(seq(-3,  5, 0.01), -1, 4))
    
    ggplot() +
        geom_line(data = x.data1, aes(x = x, y = p, color = "P1"), size = 1) +
        geom_line(data = x.data2, aes(x = x, y = p, color = "P2"), size = 1) +
        geom_line(data = x.data3, aes(x = x, y = p, color = "P3"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(" a = 0, b = 2", 
                                                            " a = -2, b = 3",
                                                            " a = 1, b = 4"
                                                            )) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
            scale_x_continuous(minor_breaks = seq(-5 , 5, 1), breaks = seq(-5, 5, 1))
        
}


#================================================================================
#    Beta distributions
#================================================================================
beta.dynamic <- function(alpha, beta) {
    
    x.data <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), alpha, beta))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" alpha =", alpha, ", beta =", beta, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(aes(color = "P"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
        scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1))
    
}


beta.static <- function() {
    
    cols <- c("P1" = "#2f4074", "P2" = "#4bb74c", "P3" = "#ba064e", "P4" =  "#c32ed4", "P5" = "#cccc00") 
    
    x.data1 <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), 0.5, 0.5))
    x.data2 <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), 5, 1))
    x.data3 <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), 1, 3))
    x.data4 <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), 2, 2))
    x.data5 <- data.frame(x = seq(0, 1, 0.01), p = dbeta(seq(0, 1, 0.01), 2, 5))
    ggplot() +
        geom_line(data = x.data1, aes(x = x, y = p, color = "P1"), size = 1) +
        geom_line(data = x.data2, aes(x = x, y = p, color = "P2"), size = 1) +
        geom_line(data = x.data3, aes(x = x, y = p, color = "P3"), size = 1) +
        geom_line(data = x.data4, aes(x = x, y = p, color = "P4"), size = 1) +
        geom_line(data = x.data5, aes(x = x, y = p, color = "P5"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(" alpha = 0.5, beta = 0.5", 
                                                            " alpha = 5, beta = 1",
                                                            " alpha = 1, beta = 3",
                                                            " alpha = 2, beta = 2",
                                                            " alpha = 2, beta = 5")) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14), 
              legend.position="bottom", legend.direction="vertical") +
        scale_x_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.1)) +
        scale_y_continuous(minor_breaks = seq(0 , 2.5, 0.5), breaks = seq(0, 2.5, 0.5), limits = c(0, 2.5))
    
}


#================================================================================
#    Exponential distributions
#================================================================================
exponential.dynamic <- function(rate) {
    
    x.data <- data.frame(x = seq(0, 8, 0.01), p = dexp(seq(0, 8, 0.01), rate))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" rate =", rate, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(aes(color = "P"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
        scale_x_continuous(minor_breaks = seq(0 , 8, 1), breaks = seq(0 , 8, 1))
    
}


exponential.static <- function() {
    
    cols <- c("P1" = "#7b3294", "P2" = "#abd9e9", "P3" = "#e66101") 
    
    x.data1 <- data.frame(x = seq(0, 8, 0.01), p = dexp(seq(0, 8, 0.01), 0.5))
    x.data2 <- data.frame(x = seq(0, 8, 0.01), p = dexp(seq(0, 8, 0.01), 1))
    x.data3 <- data.frame(x = seq(0, 8, 0.01), p = dexp(seq(0, 8, 0.01), 1.5))
    
    ggplot() +
        geom_line(data = x.data1, aes(x = x, y = p, color = "P1"), size = 1.5) +
        geom_line(data = x.data2, aes(x = x, y = p, color = "P2"), size = 1.5) +
        geom_line(data = x.data3, aes(x = x, y = p, color = "P3"), size = 1.5) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(" rate = 0.5", 
                                                            " rate = 1",
                                                            " rate = 1.5")) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
        scale_x_continuous(minor_breaks = seq(0 , 8, 1), breaks = seq(0 , 8, 1))
}

#================================================================================
#    Normal distributions
#================================================================================
normal.dynamic <- function(mean, sd) {
    
    #for calculating the density
    x.axis <- seq((mean - 5 * sd), (mean + 5 * sd), 0.01)
    x.data <- data.frame(x = x.axis, p = dnorm(x.axis, mean, sd))
    
    # calculating densities at s.d to plot vline at s.d
    x.axis.vline <- seq((mean - 3 * sd), (mean + 3 * sd), sd)
    x.data.vline <- data.frame(x = x.axis.vline, p = dnorm(x.axis.vline, mean, sd))
    
    #to color standard deviations
    sd.1 <- seq((mean - sd), (mean + sd), 0.01)
    sd.1.data <- data.frame(x = sd.1, p = dnorm(sd.1, mean, sd))
    
    sd.2.left <- seq((mean - 2 * sd), (mean - sd), 0.01)
    sd.2.data.left <- data.frame(x = sd.2.left, p = dnorm(sd.2.left, mean, sd))
    
    sd.2.right <- seq((mean + sd), (mean + 2 * sd), 0.01)
    sd.2.data.right <- data.frame(x = sd.2.right, p = dnorm(sd.2.right, mean, sd))
    
    sd.3.left <- seq((mean - 3 * sd), (mean - 2 * sd), 0.01)
    sd.3.data.left <- data.frame(x = sd.3.left, p = dnorm(sd.3.left, mean, sd))
    
    sd.3.right <- seq((mean + 2 * sd), (mean + 3 * sd), 0.01)
    sd.3.data.right <- data.frame(x = sd.3.right, p = dnorm(sd.3.right, mean, sd))
    
    cols <- c("P" = "#ba064e")
    param.str <- paste(" mean =", mean, ", sd =", sd, sep = " ")
    
    ggplot(data = x.data, aes(x = x, y = p)) +
        geom_line(aes(color = "P"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(param.str)) +
        geom_linerange(data = x.data.vline, aes(ymax=p,ymin=0),size= 0.8, color = "#ba064e") +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_line(colour = "black", size = 0.05),
              panel.grid.minor =  element_line(colour = "black", size = 0.05),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
        scale_x_continuous(minor_breaks = seq((mean - 5 * sd) , (mean + 5 * sd), sd),
                           breaks = seq((mean - 5 * sd) , (mean + 5 * sd), sd)) +
        geom_ribbon(data = sd.1.data, aes(ymax = p, ymin = 0, x = x), alpha = 0.6, fill = "#0570b0") +
        geom_ribbon(data = sd.2.data.right, aes(ymax = p, ymin = 0, x = x), alpha = 0.6, fill = "#74a9cf") +
        geom_ribbon(data = sd.2.data.left, aes(ymax = p, ymin = 0, x = x), alpha = 0.6, fill = "#74a9cf") +
        geom_ribbon(data = sd.3.data.right, aes(ymax = p, ymin = 0, x = x), alpha = 0.6, fill = "#bdc9e1") +
        geom_ribbon(data = sd.3.data.left, aes(ymax = p, ymin = 0, x = x), alpha = 0.6, fill = "#bdc9e1")
    
    
}

dnorm.calc <- function(mean, sd) {
    x.axis <- seq((mean - 5 * sd), (mean + 5 * sd), 0.01)
    x.data <- data.frame(x = x.axis, p = dnorm(x.axis, mean, sd))
    return(x.data)
}

normal.static <- function() {
    
    #for calculating the density
    x.data1 <- dnorm.calc(0, 0.4)
    x.data2 <- dnorm.calc(0, 1)
    x.data3 <- dnorm.calc(0, 5)
    x.data4 <- dnorm.calc(-2, 0.5)
    
    cols <- c("P1" = "#d7191c", "P2" = "#4dac26", "P3" = "#d01c8b", "P4" = "#2b83ba")
    
    ggplot() +
        geom_line(data = x.data1, aes(x = x, y = p, color = "P1"), size = 1) +
        geom_line(data = x.data2, aes(x = x, y = p,color = "P2"), size = 1) +
        geom_line(data = x.data3, aes(x = x, y = p,color = "P3"), size = 1) +
        geom_line(data = x.data4, aes(x = x, y = p,color = "P4"), size = 1) +
        xlab("x") + ylab("p(x)") +
        scale_colour_manual(name="",values=cols, labels = c(" mean = 0, sd = 0.4",
                                                            " mean = 0, sd = 1",
                                                            " mean = 0, sd = 5",
                                                            " mean = -2, sd = 0.5")) +
        theme_bw() +
        theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
              axis.title.x = element_text(colour="grey20",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
              axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.8,face="plain"),
              panel.grid.major =  element_blank(),
              panel.grid.minor =  element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.position="bottom", legend.direction="vertical") +
        xlim(-5, 5)
}



