library(car)
library(lme4)
library(arm)
COMPARE_EASY <- 1 # Boolean indicates if comparison is easy-mix or diff-mix
# Initialise variables
subslist <- c(1:3,5:22,24) # All subjects
#subslist <- c(3,4,5,10,11,12,20,23,24) # Subject with optimal difficult behaviour
nsubs <- length(subslist)
pdf(file=paste("results_wts_exp5.pdf", sep=""))
alldata <- NULL
all_meanwts <- NULL
all_inters <- NULL
all_sewts <- NULL
all_se_wi <- NULL
ialldata <- NULL
iall_meanwts <- NULL
iall_inters <- NULL
iall_sewts <- NULL
iall_se_wi <- NULL

bounds_getdata <- function(arg_filename, arg_subid)
{
    subtable <- read.csv(arg_filename, header=TRUE)
    subdata <- data.frame(subtable)
    subdata$individual <- rep(arg_subid, nrow(subdata))
    return(subdata)
}


bounds_plot_wts <- function(arg_stimixs, arg_meanwts, arg_inters, arg_sewts, arg_se_wi)
{
    xlab_string <- "Stimuli index"
    ylab_string = "Average regression weight"
    clfg <- adjustcolor(palette(), alpha.f = 1) # Color for mean estimates
    clbg <- adjustcolor(palette(), alpha.f = 0.12) # Color confidence envelopes

    se_e_L <- arg_meanwts[,1] - (arg_sewts[,1] / 2)
    se_e_U <- arg_meanwts[,1] + (arg_sewts[,1] / 2)
    se_wi_e_L <- arg_meanwts[,1] - (arg_se_wi[,1] / 2)
    se_wi_e_U <- arg_meanwts[,1] + (arg_se_wi[,1] / 2)
#    se_d_L <- arg_meanwts[,2] - (arg_sewts[,2] / 2)
#    se_d_U <- arg_meanwts[,2] + (arg_sewts[,2] / 2)
    se_m_L <- arg_meanwts[,2] - (arg_sewts[,2] / 2)
    se_m_U <- arg_meanwts[,2] + (arg_sewts[,2] / 2)
    se_wi_m_L <- arg_meanwts[,2] - (arg_se_wi[,2] / 2)
    se_wi_m_U <- arg_meanwts[,2] + (arg_se_wi[,2] / 2)

    # Plot regression coefficients
    plot(arg_stimixs, arg_meanwts[,1], type="l",
          xlab=xlab_string, ylab=ylab_string, ylim=c(0,6), col=clfg[1])
    polygon(c(arg_stimixs,rev(arg_stimixs)),
            c(se_e_L,rev(se_e_U)),
            col=clbg[1], border = FALSE)
    segments(arg_stimixs, se_wi_e_L, arg_stimixs, se_wi_e_U, lwd=2, col=clfg[1])
    segments(arg_stimixs - 0.1, se_wi_e_L, arg_stimixs + 0.1, se_wi_e_L, lwd=2, col=clfg[1])
    segments(arg_stimixs - 0.1, se_wi_e_U, arg_stimixs + 0.1, se_wi_e_U, lwd=2, col=clfg[1])
    points(arg_stimixs, arg_meanwts[,1], pch=16, col=clfg[1])
    lines(arg_stimixs, arg_meanwts[,2], col=clfg[2])
    polygon(c(arg_stimixs,rev(arg_stimixs)),
            c(se_m_L,rev(se_m_U)),
            col=clbg[2], border = FALSE)
    segments(arg_stimixs, se_wi_m_L, arg_stimixs, se_wi_m_U, lwd=2, col=clfg[2])
    segments(arg_stimixs - 0.1, se_wi_m_L, arg_stimixs + 0.1, se_wi_m_L, lwd=2, col=clfg[2])
    segments(arg_stimixs - 0.1, se_wi_m_U, arg_stimixs + 0.1, se_wi_m_U, lwd=2, col=clfg[2])
    points(arg_stimixs, arg_meanwts[,2], pch=16, col=clfg[2])
#    lines(arg_stimixs, arg_meanwts[,2], col=clfg[3])
#    polygon(c(arg_stimixs,rev(arg_stimixs)),
#            c(se_d_L,rev(se_d_U)),
#            col=clbg[2], border = FALSE)
#    points(arg_stimixs, arg_meanwts[,2], pch=16, col=clfg[3])
    legend('topright',
#           c("easy", "mixed", "diff"),
           c("easy", "mixed"),
           lty=1, lwd=2,
           col=clfg[1:2])
    title(main = "Stimuli weights, Experiment 5", cex=1.5)

    # Plot the probabilities
    pup_e <- invlogit(arg_inters[,1] + arg_meanwts[,1]) # Pr(Up|1) = invlogit(inter + slope*1)
    pup_m <- invlogit(arg_inters[,2] + arg_meanwts[,2]) # Pr(Up|1) = invlogit(inter + slope*1)
    plot(arg_stimixs, pup_e, type="l",
          xlab=xlab_string, ylab="P(Up|S_Up)", ylim=c(0,1), col=clfg[1])
    points(arg_stimixs, pup_e, pch=16, col=clfg[1])
    lines(arg_stimixs, pup_m, col=clfg[2])
    points(arg_stimixs, pup_m, pch=16, col=clfg[2])
    legend('topright',
#           c("easy", "mixed", "diff"),
           c("easy", "mixed"),
           lty=1, lwd=2,
           col=clfg[1:2])
    title(main = "Stimuli weights, Experiment 5", cex=1.5)
}

# Aggregate data for subjects
for (ix in subslist) {
    subdata <- bounds_getdata(paste("nofold_sub_", ix, ".csv", sep=""), ix)
    alldata <- rbind(alldata, subdata)
    print(ix)

    rm(subdata)
}

### Stim wt analysis

for (ii in seq(1,8,by=1)) { # for first 10 stims
    wtlogit_e <- glmer(decision ~ stimulus + (1 + stimulus | individual),
                       data=alldata[alldata$time==ii & alldata$condition==1,],
                       family=binomial(link="logit"))
    wtlogit_m <- glmer(decision ~ stimulus + (1 + stimulus | individual),
                       data=alldata[alldata$time==ii & alldata$condition==3,],
                       family=binomial(link="logit"))
    iwtlogit_e <- glmer(decision ~ stimulus + (1 + stimulus | individual),
                       data=alldata[alldata$invtime==ii & alldata$condition==1,],
                       family=binomial(link="logit"))
    iwtlogit_m <- glmer(decision ~ stimulus + (1 + stimulus | individual),
                       data=alldata[alldata$invtime==ii & alldata$condition==3,],
                       family=binomial(link="logit"))

    ### Get average effect of stimuli ii across participants
    inter_e <- fixef(wtlogit_e)[1]
    meanwt_e <- fixef(wtlogit_e)[2] # Fixed effect for regression slope
    sewt_e <- se.fixef(wtlogit_e)[2] # Std Error for regression slope
    se_wi_e <- attributes(VarCorr(wtlogit_e)$individual)$stddev[2] # Within-individual variance
    inter_m <- fixef(wtlogit_m)[1]
    meanwt_m <- fixef(wtlogit_m)[2]
    sewt_m <- se.fixef(wtlogit_m)[2]
    se_wi_m <- attributes(VarCorr(wtlogit_m)$individual)$stddev[2]

    ### Concatenate
    inters_ii <- c(inter_e, inter_m)
    meanwts_ii <- c(meanwt_e, meanwt_m)
    sewts_ii <- c(sewt_e, sewt_m)
    se_wi_ii <- c(se_wi_e, se_wi_m)
    all_inters <- rbind(all_inters, inters_ii)
    all_meanwts <- rbind(all_meanwts, meanwts_ii)
    all_sewts <- rbind(all_sewts, sewts_ii)
    all_se_wi <- rbind(all_se_wi, se_wi_ii)

    ### Aligned at decision
    iinter_e <- fixef(iwtlogit_e)[1] # Intercept
    imeanwt_e <- fixef(iwtlogit_e)[2] # Fixed effect for regression slope
    isewt_e <- se.fixef(iwtlogit_e)[2] # Std Error for regression slope
    ise_wi_e <- attributes(VarCorr(iwtlogit_e)$individual)$stddev[2] # Within-individual variance
    iinter_m <- fixef(iwtlogit_m)[1]
    imeanwt_m <- fixef(iwtlogit_m)[2]
    isewt_m <- se.fixef(iwtlogit_m)[2]
    ise_wi_m <- attributes(VarCorr(iwtlogit_m)$individual)$stddev[2]

    iinters_ii <- c(iinter_e, iinter_m)
    imeanwts_ii <- c(imeanwt_e, imeanwt_m)
    isewts_ii <- c(isewt_e, isewt_m)
    ise_wi_ii <- c(ise_wi_e, ise_wi_m)
    iall_inters <- rbind(iall_inters, iinters_ii)
    iall_meanwts <- rbind(iall_meanwts, imeanwts_ii)
    iall_sewts <- rbind(iall_sewts, isewts_ii)
    iall_se_wi <- rbind(iall_se_wi, ise_wi_ii)
}

bounds_plot_wts(seq(1,nrow(all_meanwts)), all_meanwts, all_inters, all_sewts, all_se_wi)
bounds_plot_wts(seq(1,nrow(iall_meanwts)), iall_meanwts, iall_inters, iall_sewts, iall_se_wi)

#save(list=ls(), file=paste("results_allsubs_gono.RData", sep=""))

#mc_data <- data.frame(ml_slopes_e, ml_intercept_e, ml_slopes_m, ml_intercept_m)
#write.csv(mc_data, file="mcdata_exp3.csv", row.names=FALSE)

dev.off()
