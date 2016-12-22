library(car)
#library(lme4)
library(arm)
COMPARE_EASY <- 1 # Boolean indicates if comparison is easy-mix or diff-mix

# Initialise variables
subslist <- c(1:24) # All subjects
nsubs <- length(subslist)
pdf(file=paste("ndstims_exp6.pdf", sep=""))

bounds_getdata <- function(arg_filename, arg_subid)
{
    subtable <- read.csv(arg_filename, header=TRUE)
    subdata <- data.frame(subtable)
    subdata$individual <- rep(arg_subid, nrow(subdata))
    return(subdata)
}


bounds_plot_wts <- function(arg_subid, arg_coef, arg_se)
{
    xlab_string <- "Stimuli index"
    ylab_string = "Regression weight"
    clfg <- adjustcolor(palette(), alpha.f = 1) # Color for mean estimates
    clbg <- adjustcolor(palette(), alpha.f = 0.12) # Color confidence envelopes

    xind <- seq(1,length(arg_coef)-1)
    coefs.plot <- arg_coef[2:length(arg_coef)]
    se.plot <- arg_se[2:length(arg_se)]
    se_L <- coefs.plot - (se.plot / 2)
    se_U <- coefs.plot + (se.plot / 2)

    plot(xind, coefs.plot, type="l",
          xlab=xlab_string, ylab=ylab_string, ylim=c(-1,8), col=clfg[1])
    segments(xind, se_L, xind, se_U, lwd=2, col=clfg[1])
    segments(xind - 0.1, se_L, xind + 0.1, se_L, lwd=2, col=clfg[1])
    segments(xind - 0.1, se_U, xind + 0.1, se_U, lwd=2, col=clfg[1])
    points(xind, coefs.plot, pch=16, col=clfg[1])

    title(main = paste("Stim Wts in Exp 6: Participant ", arg_subid, sep=""), cex=1.5)
}

# Compute regression coefs for each participant
max.ixs <- NULL
for (ix in subslist) {
    subdata <- bounds_getdata(paste("nofold2_sub_", ix, ".csv", sep=""), ix)
    fit.e <- glm(decision ~ r1+r2+r3+r4+r5+r6,
                 data=subdata[subdata$condition==1,],
                 family=binomial(link="logit"))
    fit.m <- glm(decision ~ r1+r2+r3+r4+r5+r6,
                 data=subdata[subdata$condition==3,],
                 family=binomial(link="logit"))
    bounds_plot_wts(ix, coef(fit.m), se.coef(fit.m))
    rmax <- which.max(coef(fit.m)[2:length(coef(fit.m))])
    attributes(rmax) <- NULL
    print(paste("Participant: ", ix, "; Max R: ", rmax))
    max.ixs <- cbind(max.ixs, rmax)

    rm(subdata)
}

write.csv(max.ixs, file="ndstims_exp6.csv", row.names=FALSE)

dev.off()
