# Einfaches R Skript zur Erzeugung von Wellenform, die bei Überlaguerung zweier Sinuswellen entsteht
# Geschrieben von Melanie Stefan für die VL Physik für Mediziner*innen, 2022
# CC BY-SA 4.0


library(ggplot2)
library(tidyr)

# Fall 1: Schwebung

x=seq(0,50*pi,0.1)
w1 = sin(x)
w2 = sin(x*0.9)
w_sum = w1+w2

all <- data.frame(x,w1,w2,w_sum)
all_long <-  gather(all, "wave", "y", c(w1,w2,w_sum))
all_long[all_long$wave=="w_sum","wave"] <- "w1+w2"
all_long$wave=factor(all_long$wave, levels=c("w1","w2","w1+w2"))

p <- ggplot(all_long, aes (x=x, y=y, col=wave)) 
p <- p + geom_line(show.legend=FALSE) 
p <- p + facet_grid(rows=vars(wave))
p <- p + scale_color_brewer(palette="Set2")
p <- p + ylim(c(-2,2))


png("schwebung.png", width = 15, height = 9, units = "cm", res = 300)
p
dev.off()


# Fall 2: w1 Vielfaches von w2

x=seq(0,50*pi,0.1)
w1 = sin(x)
w2 = sin(x/2)
w_sum = w1+w2

all <- data.frame(x,w1,w2,w_sum)
all_long <-  gather(all, "wave", "y", c(w1,w2,w_sum))
all_long[all_long$wave=="w_sum","wave"] <- "w1+w2"
all_long$wave=factor(all_long$wave, levels=c("w1","w2","w1+w2"))

p1 <- ggplot(all_long, aes (x=x, y=y, col=wave)) 
p1 <- p1 + geom_line(show.legend=FALSE) 
p1 <- p1 + facet_grid(rows=vars(wave))
p1 <- p1 + scale_color_brewer(palette="Set2")
p1 <- p1 + ylim(c(-2,2))

png("vielfache.png", width = 15, height = 9, units = "cm", res = 300)
p1
dev.off()



# Fall 3: Positive Interferenz

x=seq(0,50*pi,0.1)
w1 = sin(x)
w2 = sin(x)
w_sum = w1+w2

all <- data.frame(x,w1,w2,w_sum)
all_long <-  gather(all, "wave", "y", c(w1,w2,w_sum))
all_long[all_long$wave=="w_sum","wave"] <- "w1+w2"
all_long$wave=factor(all_long$wave, levels=c("w1","w2","w1+w2"))

p2 <- ggplot(all_long, aes (x=x, y=y, col=wave)) 
p2 <- p2 + geom_line(show.legend=FALSE) 
p2 <- p2 + facet_grid(rows=vars(wave))
p2 <- p2 + scale_color_brewer(palette="Set2")
p2 <- p2 + ylim(c(-2,2))

p2

png("positive_interferenz.png", width = 15, height = 9, units = "cm", res = 300)
p2
dev.off()


# Fall 4: Negative Interferenz

x=seq(0,50*pi,0.1)
w1 = sin(x)
w2 = sin(x+pi)
w_sum = w1+w2

all <- data.frame(x,w1,w2,w_sum)
all_long <-  gather(all, "wave", "y", c(w1,w2,w_sum))
all_long[all_long$wave=="w_sum","wave"] <- "w1+w2"
all_long$wave=factor(all_long$wave, levels=c("w1","w2","w1+w2"))

p3 <- ggplot(all_long, aes (x=x, y=y, col=wave)) 
p3 <- p3 + geom_line(show.legend=FALSE) 
p3 <- p3 + facet_grid(rows=vars(wave))
p3 <- p3 + scale_color_brewer(palette="Set2")
p3 <- p3 + ylim(c(-2,2))
p3

png("negative_interferenz.png", width = 15, height = 9, units = "cm", res = 300)
p3
dev.off()


# Fall 5: Interferenz dazwischen

x=seq(0,50*pi,0.1)
w1 = sin(x)
w2 = sin(x+pi/3)
w_sum = w1+w2

all <- data.frame(x,w1,w2,w_sum)
all_long <-  gather(all, "wave", "y", c(w1,w2,w_sum))
all_long[all_long$wave=="w_sum","wave"] <- "w1+w2"
all_long$wave=factor(all_long$wave, levels=c("w1","w2","w1+w2"))

p4 <- ggplot(all_long, aes (x=x, y=y, col=wave)) 
p4 <- p4 + geom_line(show.legend=FALSE) 
p4 <- p4 + facet_grid(rows=vars(wave))
p4 <- p4 + scale_color_brewer(palette="Set2")
p4 <- p4 + ylim(c(-2,2))


png("zwischen_interferenz.png", width = 15, height = 9, units = "cm", res = 300)
p4
dev.off()


