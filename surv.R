library(openxlsx)
library(dplyr)
library(survival)
library(survminer)
library(ggtext)

#' @name c2i
#' @title Convert a range of Excel columns to integers
#' @description Converts a series of Excel column labels to integers.
#' @param x A character string
#' @export
#' @examples
#' c2i("A:C F L:O")
c2i <- function(x) {
  if (!is.character(x)) {
    stop("x must be character")
  }
  x <- toupper(x)
  u <- strsplit(x, split=" ", fixed=TRUE)[[1]]
  p <- strsplit(u, split=":", fixed=TRUE)
  q <- c()
  for (i in 1:length(p)) {
    if (length(p[[i]]) == 1) q <- c(q, col2int(p[[i]]))
    else q <- c(q, col2int(p[[i]][1]):col2int(p[[i]][length(p[[i]])]))
  }
  q
}


f2 <- read.xlsx("Table S1_Survival experiment.xlsx", sheet="O. flavus 200 m", rows=52:70, cols=c2i("b:l"))
f5 <- read.xlsx("Table S1_Survival experiment.xlsx", sheet="O. flavus 500 m", rows=62:85, cols=c2i("b:j"))
a3 <- round(read.xlsx("Table S1_Survival experiment.xlsx", sheet="O. albinus 300 m", rows=62:83, cols=c2i("b:l")))
a5 <- read.xlsx("Table S1_Survival experiment.xlsx", sheet="O. albinus 500 m", rows=63:84, cols=c2i("b:l"))

restr <- function(data, label, group) {
  sdf <- select(data[-1], starts_with(label))
  t <- rep(data$t, ncol(sdf))
  tem <- data.frame(c("a", "b", "c", "d"), 1:4, 1:4, rep(3, 4))
  colnames(tem) <- c("group", "start", "end", "status")
  j <- 0
  for (nc in 1:ncol(sdf)) {
    x <- sdf[,nc]
    for (i in 2:length(x)) {
      if (x[i] < x[i-1]) {
        plus <- x[i-1] - x[i]
        tem[(j+1):(j+plus),] <- data.frame(group, t[i-1], t[i], 1)
        j <- j + plus
      }
    }
    if (x[i] > 0) {
      plus <- x[i]
      tem[(j+1):(j+plus),] <- data.frame(label, t[i], t[i], 0)
      j <- j + plus
    }
  }
  tem
}

restr(data=a3, label="a", group="a3")

s <- rbind(restr(data=a3, label="a", group="a3"), restr(data=a5, label="a", group="a5"),
  restr(data=f2, label="f", group="f2"), restr(data=f5, label="f", group="f5"))
s$group <- factor(s$group, levels=c("a3", "a5", "f2", "f5"))

survdiff(Surv(time=end, event=status) ~ group, data=s[(s$group=="a3")|(s$group=="a5"),])
survdiff(Surv(time=end, event=status) ~ group, data=s[(s$group=="f2")|(s$group=="f5"),])

p.adjust(c(
survdiff(Surv(time=end, event=status) ~ group, data=s[(s$group=="a3")|(s$group=="a5"),])$pvalue,
survdiff(Surv(time=end, event=status) ~ group, data=s[(s$group=="f2")|(s$group=="f5"),])$pvalue
))

png("surv.png", units="in", width=6, height=4, res=300)
cof <- c("orange")
coa <- c("cornflowerblue")
lt <- c(3, 1, 3, 1)
par(mar=c(4,4,0.5,0.5))
plot(survfit(Surv(time=end, event=status) ~ group, data=s), xlim=c(4.8, 27.3), las=1, xlab="Temperature, °C", ylab="Survival probability", col=c(coa, coa, cof, cof), lwd=2, lty=lt, xaxt="n")
lines(c(0, 30), c(0.5, 0.5), lty=2)
#axis(side=1, at=c(4, 6:9, 11:14, 16:19, 21:24, 26), labels=FALSE, las=1, lwd=0, lwd.ticks=1, tck=-0.01)
axis(side=1, at=1:7*4)
axis(side=1, at=c(5:7, 9:11, 13:15, 17:19, 21:23, 25:27), labels=FALSE, las=1, lwd=0, lwd.ticks=1, tck=-0.01)
axis(side=2, at=1:5/5-0.1, labels=FALSE, las=1, lwd=0, lwd.ticks=1, tck=-0.01)
legend("bottomleft", legend=c(expression(paste(italic("O. flavus"), " from 200 m")), expression(paste(italic("O. flavus"), " from 500 m")), expression(paste(italic("O. albinus"), " from 300 m")), expression(paste(italic("O. albinus"), " from 500 m"))), bty="n", lty=lt, lwd=2, col=c(cof, cof, coa, coa))
dev.off()

### if points, then remove small samples!!!

ggsurvplot(survfit(Surv(time=end, event=status) ~ group, data=s), pval=TRUE, conf.int=TRUE, surv.median.line="hv")


