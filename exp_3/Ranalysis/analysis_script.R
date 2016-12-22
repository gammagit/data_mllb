library(car)
# Initialise variables
blockno <- 3 # Block to analyse (3 means block 1 & 2 combined)
subslist <- c(1:4) # All subjects
nsubs <- length(subslist)
#pdf(file=paste("plots_allsubs_block", blockno, ".pdf", sep=""))
pdf(file=paste("RR_allsubs_block", blockno, ".pdf", sep=""))
#pdf(file=paste("RR_no_outlier_block", blockno, ".pdf", sep=""))
slope_vector_e <- NULL
slope_vector_d <- NULL
slope_vector_m <- NULL
prop_vector_e <- NULL
prop_vector_m <- NULL
acc_vector_e <- NULL
acc_vector_m <- NULL
rr_vector_e <- NULL
rr_vector_m <- NULL
acc_vector_e_fastRT <- NULL
acc_vector_e_slowRT <- NULL
acc_vector_m_fastRT <- NULL
acc_vector_m_slowRT <- NULL
RT_vector_e.mu <- NULL
RT_vector_d.mu <- NULL
RT_vector_m_e.mu <- NULL
RT_vector_m_d.mu <- NULL
RT_vector_e_cor.mu <- NULL
RT_vector_e_err.mu <- NULL
RT_vector_m_e_cor.mu <- NULL
RT_vector_m_e_err.mu <- NULL
latency_vector_e.sigma <- NULL
latency_vector_d.sigma <- NULL
latency_vector_m.sigma <- NULL
evidence_vector_e.mu <- NULL
evidence_vector_d.mu <- NULL
evidence_vector_m.mu <- NULL
evidence_vector_e.sigma <- NULL
evidence_vector_d.sigma <- NULL
evidence_vector_m.sigma <- NULL
total_time_easy <- 120*4 # Total time spent in Easy blocks (same for all subs)
total_time_diff <- 120*4
total_time_mix <- 120*4

bounds_getdata <- function(arg_filename)
{
    subtable <- read.csv(arg_filename, header=TRUE)
    subdata <- data.frame(subtable)
    evud <- (-1) * subdata$evidence * ((subdata$decision*2)-1)
    subdata$evidence_ud <- evud
    evidence_cat <- 1*(abs(subdata$evidence) < abs(subdata$high_evidence))
    subdata$evidence_cat <- evidence_cat
    remove(evidence_cat)
    return(subdata)
}

bounds_plotsubdata <- function(arg_data)
{
    mycols <- adjustcolor(palette(colorRampPalette(c("#a96faf", "#DD693E", "#4abb6c"))(3)),
                          alpha.f = 0.8)
    opal <- palette(mycols)
    palette(opal)
    par(mar=c(5.1,5.1,4.1,2.1))
    scatterplot(jitter(evidence_ud,0.5) ~ jitter(time,0.5) | condition,
                data=arg_data,
                lwd=2,
                boxplots='',
                xlab='time', ylab='evidence',
                legend.coords="topright",
                reg.line=rlm,
                smoother=FALSE,
                xlim=c(0, 50), ylim=c(-5, 10),
                pch=c(16,4,21),
                cex=1.5,
                cex.lab=1.5,
                lty=1:3)
    palette("default")
    bounds_plothist(arg_data)
}

bounds_plothist <- function(arg_data)
{
    # Plot RTs hist for Easy
    rts_easy <- with(arg_data, time[condition=='easy'])
    hist_easy <- hist(rts_easy, breaks=c(seq(0,100,by=1)), plot=FALSE)
    hist_easy$counts <- (-1) * hist_easy$counts * 0.01
    par(new=T)
    plot(hist_easy, axes=F, xlab="", ylab="", main="",
         col="#DD693E", border="#DD693E",
         xlim=c(0,50)*(50/50)-0.5, ylim=c(-2,0)-0.08)
    # Plot RTs hist for Mix
    rts_mix <- with(arg_data, time[condition=='mix'])
    hist_mix <- hist(rts_mix, breaks=c(seq(0,100,by=1)), plot=FALSE)
    hist_mix$counts <- hist_mix$counts * 0.01;
    par(new=T)
    plot(hist_mix, axes=F, xlab="", ylab="", main="",
         col="#4abb6c", border="#4abb6c",
         xlim=c(0,50)*(50/50)-0.5, ylim=c(0,2)+0.08)
}

bounds_plot_evdhist <- function(arg_data)
{
    par(mfrow=c(3,1))
    par(mar=c(5.1,5.1,4.1,2.1))
    # Easy condition
    evd_easy <- with(arg_data, evidence_ud[condition=='easy'])
    hist_evd_easy <- hist(evd_easy, breaks=c(-10.5:25.5), plot=FALSE)
    plot(hist_evd_easy,
         axes=T,
         cex.axis=1.5,
         xlab="",
         ylab="No. of trials",
         cex.lab=2,
         main="Easy",
         cex.main=2,
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,35))
    abline(v=3, lwd=4, lty="longdash", col="black")
    # Difficult condition
    evd_diff <- with(arg_data, evidence_ud[condition=='diff'])
    hist_evd_diff <- hist(evd_diff, breaks=c(-10.5:25.5), plot=FALSE)
    plot(hist_evd_diff,
         axes=T,
         cex.axis=1.5,
         xlab="",
         ylab="No. of trials",
         cex.lab=2,
         main="Difficult",
         cex.main=2,
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,35))
    abline(v=0, lwd=4, lty="longdash", col="black")
    # Mixed condition
    evd_mix <- with(arg_data, evidence_ud[condition=='mix'])
    hist_evd_mix <- hist(evd_mix, breaks=c(-10.5:25.5), plot=FALSE)
    plot(hist_evd_mix,
         axes=T,
         cex.axis=1.5,
         xlab="Evidence at decision",
         ylab="No. of trials",
         cex.lab=2,
         main="Mixed",
         cex.main=2,
#         col="#4abb6c",
#         border="#4abb6c",
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,35))
    # Reset graph
    par(mfrow=c(1,1))
    par(mar=c(5.1,4.1,4.1,2.1))
}

bounds_plot_avgevdhist <- function(arg_evd_e, arg_evd_d, arg_evd_m)
{
    # Plot Evidence hist for Easy
    par(mfrow=c(3,1))
    par(mar=c(5.1,5.1,4.1,2.1))
    hist_evd_easy <- hist(arg_evd_e, breaks=c(-0.5:5.5), plot=FALSE)
    plot(hist_evd_easy,
         axes=T,
         cex.axis=1.5,
         xlab="",
         ylab="No. of subjects",
         cex.lab=2,
         main="Easy",
         cex.main=2,
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,14))
    abline(v=3, lwd=4, lty="longdash", col="black")
    # Difficult
    hist_evd_diff <- hist(arg_evd_d, breaks=c(-0.5:5.5), plot=FALSE)
    plot(hist_evd_diff,
         axes=T,
         cex.axis=1.5,
         xlab="",
         ylab="No. of subjects",
         cex.lab=2,
         main="Difficult",
         cex.main=2,
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,14))
    abline(v=0, lwd=4, lty="longdash", col="black")
    # Mixed
    hist_evd_mix <- hist(arg_evd_m, breaks=c(-0.5:5.5), plot=FALSE)
    plot(hist_evd_mix,
         axes=T,
         cex.axis=1.5,
         xlab="Mean Evidence",
         ylab="No. of subjects",
         cex.lab=2,
         main="Mixed",
         cex.main=2,
         col="darkgray",
         border="darkgray",
         xlim=c(-5,7),
         ylim=c(0,14))
    par(mfrow=c(1,1))
    par(mar=c(5.1,4.1,4.1,2.1))
}

bounds_fitlm <- function(arg_data)
{
    # Linear regression
    lmfit_e <- lm(evidence_ud[condition=='easy'] ~ time[condition=='easy'],
                  data=arg_data)
    lmfit_d <- lm(evidence_ud[condition=='diff'] ~ time[condition=='diff'],
                  data=arg_data)
    lmfit_m <- lm(evidence_ud[condition=='mix'] ~ time[condition=='mix'],
                  data=arg_data)
    # Robust linear regression
    rlmfit_e <- rlm(evidence_ud[condition=='easy'] ~ time[condition=='easy'],
                    maxit=50, data=arg_data)
    rlmfit_d <- rlm(evidence_ud[condition=='diff'] ~ time[condition=='diff'],
                    maxit=50, data=arg_data)
    rlmfit_m <- rlm(evidence_ud[condition=='mix'] ~ time[condition=='mix'],
                    maxit=50, data=arg_data)
    returnlist <- list("re" = rlmfit_e, "rd" = rlmfit_d, "rm" = rlmfit_m,
                       "le" = lmfit_e, "ld" = lmfit_d, "lm" = lmfit_m)
    return(returnlist)
}

bounds_getpropdec <- function(arg_data)
{
    # Calculate proportion of trials in which decision is made at lower
    # value of evidence than hightest evidence during the trial
    prop_dec_easy <- sum(arg_data$evidence_cat==1 & arg_data$condition=='easy') /
                     sum(arg_data$condition=='easy')
    prop_dec_mix <- sum(arg_data$evidence_cat==1 & arg_data$condition=='mix') /
                    sum(arg_data$condition=='mix')
    returnlist <- list("easy" = prop_dec_easy, "mix" = prop_dec_mix)
    return(returnlist)
}

bounds_analyse_evd <- function(arg_data)
{
    easy.mu <- mean(arg_data$evidence_ud[arg_data$condition=='easy'])
    easy.sigma <- sd(arg_data$evidence_ud[arg_data$condition=='easy'])
    diff.mu <- mean(arg_data$evidence_ud[arg_data$condition=='diff'])
    diff.sigma <- sd(arg_data$evidence_ud[arg_data$condition=='diff'])
    mix.mu <- mean(arg_data$evidence_ud[arg_data$condition=='mix'])
    mix.sigma <- sd(arg_data$evidence_ud[arg_data$condition=='mix'])
    returnlist <- list("easy.mu" = easy.mu, "easy.sigma" = easy.sigma,
                       "diff.mu" = diff.mu, "diff.sigma" = diff.sigma,
                       "mix.mu" = mix.mu, "mix.sigma" = mix.sigma)
    return(returnlist)
}

bounds_dec_stats <- function(arg_data)
{
    ncorrect_easy = sum(1*(((arg_data$decision[arg_data$condition=='easy'] == 1) &
                            (arg_data$coherence[arg_data$condition=='easy'] == 0.75)) |
                           ((arg_data$decision[arg_data$condition=='easy'] == 0) &
                            (arg_data$coherence[arg_data$condition=='easy'] == 0.25))))
    ntotal_easy = sum(1*(arg_data$condition=='easy'))
    ncorrect_mix = sum(1*(((arg_data$decision[arg_data$condition=='mix'] == 1) &
                           (arg_data$coherence[arg_data$condition=='mix'] == 0.75)) |
                          ((arg_data$decision[arg_data$condition=='mix'] == 0) &
                           (arg_data$coherence[arg_data$condition=='mix'] == 0.25))))
    ntotal_mix = sum(1*(arg_data$coherence[arg_data$condition=='mix'] == 0.75 |
                        arg_data$coherence[arg_data$condition=='mix'] == 0.25))
    nhard_mix <- sum(arg_data$coherence[arg_data$condition=='mix'] == 0.53 | 
                     arg_data$coherence[arg_data$condition=='mix'] == 0.47)
    returnlist <- list("nc_e" = ncorrect_easy, "nt_e" = ntotal_easy,
                       "nc_m" = ncorrect_mix, "nt_m" = ntotal_mix,
                       "nh_m" = nhard_mix)
    return(returnlist)
}

bounds_getacc_rr <- function(arg_data)
{
    dec_stats <- bounds_dec_stats(arg_data)
    # Calculate accuracy
    acc_easy <- dec_stats$nc_e / dec_stats$n_e
    acc_mix <- dec_stats$nc_m / dec_stats$nt_m

    # Calculate reward rate
    total_reward_easy <- dec_stats$nc_e
    total_reward_mix <- dec_stats$nc_m + (0.5 * dec_stats$nh_m)
    rr_easy <- total_reward_easy / total_time_easy
    rr_mix <- total_reward_mix / total_time_mix
    returnlist <- list("acc_easy" = acc_easy, "acc_mix" = acc_mix,
                       "rr_easy" = rr_easy, "rr_mix" = rr_mix)
    return(returnlist)
}

bounds_btest <- function(arg_data, blockno)
{
    dec_stats <- bounds_dec_stats(arg_data)
    btest_easy <- binom.test(dec_stats$nc_e, dec_stats$nt_e, 1/2)
    btest_mix <- binom.test(dec_stats$nc_m, dec_stats$nt_m, 1/2)
    if(btest_easy$p.value > 0.05) {
        print(paste("Subject ", ix,
                    " failed Binomial test in Easy condition, block ",
                    blockno, sep=""))
    }
    if(btest_mix$p.value > 0.05) {
        print(paste("Subject ", ix,
                    " failed Binomial test in Mix condition, block ",
                    blockno, sep=""))
    }
}

bounds_getRT <- function(arg_data)
{
    rt_easy.mu <- mean(arg_data$time[arg_data$condition=='easy'])
    rt_easy.sd <- sd(arg_data$time[arg_data$condition=='easy'])
    rt_easy.med <- median(arg_data$time[arg_data$condition=='easy'])
    rt_diff.mu <- mean(arg_data$time[arg_data$condition=='diff'])
    rt_diff.sd <- sd(arg_data$time[arg_data$condition=='diff'])
    rt_diff.med <- median(arg_data$time[arg_data$condition=='diff'])
    rt_mix.mu <- mean(arg_data$time[arg_data$condition=='mix' ])
    rt_mix.sd <- sd(arg_data$time[arg_data$condition=='mix'])
    rt_mix.med <- median(arg_data$time[arg_data$condition=='mix'])
    rt_mixblock_easy.med <- median(arg_data$time[arg_data$condition=='mix'&
                                                 (arg_data$coherence == 0.75 |
                                                  arg_data$coherence == 0.25)])
    rt_mixblock_diff.med <- median(arg_data$time[arg_data$condition=='mix'&
                                                 (arg_data$coherence == 0.53 |
                                                 arg_data$coherence == 0.47)])
    rt_mixblock_easy.mu <- mean(arg_data$time[arg_data$condition=='mix' &
                                              (arg_data$coherence == 0.75 |
                                               arg_data$coherence == 0.25)])
    rt_mixblock_diff.mu <- mean(arg_data$time[arg_data$condition=='mix' &
                                              arg_data$coherence == 0.5])
    rt_easyblock_cor.mu <- with(arg_data,
                                mean(time[condition=='easy' &
                                          (coherence == 0.75 & decision == 1) |
                                          (coherence == 0.25 & decision == 0)]))
    rt_easyblock_err.mu <- with(arg_data,
                               mean(time[condition=='easy' &
                                         (coherence == 0.75 & decision == 0) |
                                         (coherence == 0.25 & decision == 1)]))
    rt_mixblock_easy_cor.mu <- with(arg_data,
                                    mean(time[condition=='mix' &
                                              (coherence == 0.75 & decision == 1) |
                                              (coherence == 0.25 & decision == 0)]))
    rt_mixblock_easy_err.mu <- with(arg_data,
                                    mean(time[condition=='mix' &
                                              (coherence == 0.75 & decision == 0) |
                                              (coherence == 0.25 & decision == 1)]))
    returnlist <- list("e.mu" = rt_easy.mu, "e.sd" = rt_easy.sd, "e.med" = rt_easy.med,
                       "m.mu" = rt_mix.mu, "m.sd" = rt_mix.sd, "m.med" = rt_mix.med,
                       "d.mu" = rt_diff.mu, "d.sd" = rt_diff.sd, "d.med" = rt_diff.med,
                       "m_e.mu" = rt_mixblock_easy.mu, "m_d.mu" = rt_mixblock_diff.mu,
                       "m_e.med" = rt_mixblock_easy.med, "m_d.med" = rt_mixblock_diff.med,
                       "e_cor.mu" = rt_easyblock_cor.mu, "e_err.mu" = rt_easyblock_err.mu,
                       "m_e_cor.mu" = rt_mixblock_easy_cor.mu, "m_e_err.mu" = rt_mixblock_easy_err.mu)
    return(returnlist)
}

bounds_getacc_at_RTs <- function(arg_data)
{
    sub_RT <- bounds_getRT(arg_data)

    ncor_easy_fastRT <- with(arg_data,
                             sum(1*(condition == 'easy' & time < sub_RT$e.med &
                                    ((coherence == 0.75 & decision == 1) |
                                     (coherence == 0.25 & decision == 0)))))
    ncor_easy_slowRT <- with(arg_data,
                             sum(1*(condition == 'easy' & time >= sub_RT$e.med &
                                    ((coherence == 0.75 & decision == 1) |
                                     (coherence == 0.25 & decision == 0)))))
    ntot_easy_fastRT <- with(arg_data,
                             sum(1*(condition=='easy' & time < sub_RT$e.med)))
    ntot_easy_slowRT <- with(arg_data,
                             sum(1*(condition=='easy' & time >= sub_RT$e.med)))
    acc_easy_fastRT <- ncor_easy_fastRT / ntot_easy_fastRT
    acc_easy_slowRT <- ncor_easy_slowRT / ntot_easy_slowRT
    # Mixed condition
    ncor_mix_fastRT <- with(arg_data,
                             sum(1*(condition == 'mix' & time < sub_RT$m_e.med &
                                    ((coherence == 0.75 & decision == 1) |
                                     (coherence == 0.25 & decision == 0)))))
    ncor_mix_slowRT <- with(arg_data,
                             sum(1*(condition == 'mix' & time >= sub_RT$m_e.med &
                                    ((coherence == 0.75 & decision == 1) |
                                     (coherence == 0.25 & decision == 0)))))
    ntot_mix_fastRT <- with(arg_data,
                             sum(1*(condition=='mix' & time < sub_RT$m_e.med &
                                    (coherence == 0.75 | coherence == 0.25))))
    ntot_mix_slowRT <- with(arg_data,
                             sum(1*(condition=='mix' & time >= sub_RT$m_e.med &
                                    (coherence == 0.75 | coherence == 0.25))))
    acc_mix_fastRT <- ncor_mix_fastRT / ntot_mix_fastRT
    acc_mix_slowRT <- ncor_mix_slowRT / ntot_mix_slowRT

    returnlist <- list("e_fast" = acc_easy_fastRT, "e_slow" = acc_easy_slowRT,
                       "m_fast" = acc_mix_fastRT, "m_slow" = acc_mix_slowRT)
    return(returnlist)
}

bounds_qqplot <- function(arg_x1, arg_x2)
{
    mu1 = mean(arg_x1)
    sd1 = sd(arg_x1)
    arg_x1.ord <- sort(arg_x1)
    quant <- seq(1:length(arg_x1))/length(arg_x1)
    arg_x1.fit <- qnorm(quant, mean=mu1, sd=sd1)
    plot(arg_x1.fit, arg_x1.ord, xlab="Expected quantiles", ylab="Sample quantiles")
    abline(0,1)
    title(main = "Q-Q plot 1")

    # Plot Q-Q plot for mixed
    mu2 = mean(arg_x2)
    sd2 = sd(arg_x2)
    arg_x2.ord <- sort(arg_x2)
    quant <- seq(1:length(arg_x2))/length(arg_x2)
    arg_x2.fit <- qnorm(quant, mean=mu2, sd=sd2)
    plot(arg_x2.fit, arg_x2.ord, xlab="Expected quantiles", ylab="Sample quantiles")
    abline(0,1)
    title(main = "Q-Q plot 2")
}

bounds_plot_slopes <- function(arg_slopes_e, arg_slopes_m)
{
    diff.data <- data.frame("slope" = c(arg_slopes_e, arg_slopes_m),
                            "condition" = c(rep("easy",length(arg_slopes_e)),
                                            rep("mix",length(arg_slopes_m))))
    par(mar=c(5.1,5.1,4.1,2.1))
    boxplot(diff.data$slope ~ diff.data$condition,
            border="black", col="gray", boxwex=0.5, cex=1.5, cex.lab=1.5)
    par(new=TRUE)
    abline(0, 0, col="darkgray", lty="dashed")
    par(new=TRUE)
    # Redraw boxplot on top of abline
    boxplot(diff.data$slope ~ diff.data$condition,
            border="black", col="gray", boxwex=0.5, cex=1.5, cex.lab=1.5,
            ylab=expression(paste("Regression coefficient ", beta[1], sep="")))
    # Plot each subject's slope next to boxplots
    points(rep(0.7:1.7, each = nsubs),
           unlist(split(diff.data$slope, diff.data$condition)),
           pch = 16, col="darkgray", cex=1.2)
    title(main = "Slope (reg coeff) for each participant")
}

#bounds_plot_rrelation <- function(arg_rr, arg_slope, arg_prop_m, arg_prop_e)
#{
     ### Reward rate versus slope from regression
#    scatterplot(arg_rr ~ arg_slope, lwd=2, boxplots='xy',
#                ylab='RR in mixed trials', xlab='Slope in mixed',
#                reg.line=rlm, smoother=FALSE, pch=16)
#    rlmfit_slope_rr <- rlm(arg_rr ~ arg_slope, maxit=50)
#    cor_slope_rr <- cor(arg_slope, arg_rr)
#    cor.test(arg_slope, arg_rr)
#    mtext(paste("Regression coefficient 1: ",
#                rlmfit_slope_rr$coefficients[2], sep=""),
#          side=3, line=1, adj=1, cex=0.75)
#    mtext(paste("Correlation: ", cor_slope_rr, sep=""),
#          side=3, line=0, adj=1, cex=0.75)
#    title(main = "Relation b/w Reward rate and Slope in mixed")

     ### Comparison of proportion of decrease in evidence and reward rate
#    dprops = arg_prop_m - arg_prop_e
#    scatterplot(dprops ~ arg_rr, lwd=2, boxplots='xy',
#                xlab='RR in mixed trials',
#                ylab='Difference (mixed-easy) b/w (Proportion x_final < x_max)',
#                reg.line=rlm, smoother=FALSE, pch=16)
#    rlmfit_dprops_rr <- rlm(dprops ~ arg_rr, maxit=50)
#    cor_dprops_rr <- cor(dprops, arg_rr)
#    cor.test(dprops, arg_rr)
#    mtext(paste("Regression coefficient 1: ",
#                rlmfit_dprops_rr$coefficients[2], sep=""),
#          side=3, line=1, adj=1, cex=0.75)
#    mtext(paste("Correlation: ", cor_dprops_rr, sep=""),
#          side=3, line=0, adj=1, cex=0.75)
#    title(main = "Relation b/w Reward rate and Difference in proportions")
#}

bounds_plot_props <- function(arg_prop_vector_e, arg_prop_vector_m)
{
    plot(arg_prop_vector_e, arg_prop_vector_m, xlab="Proportion Easy",
        ylab="Proportion Mixed", xlim=c(0,0.75), ylim=c(0,0.75), pch=16)
    title(main = "Proportion of trials where Evidence at decision < Highest evidence",
        cex=1.5)
    abline(0,1)
}

# Gather stats for each subject
for (ix in subslist) {
    # Read raw data from file into data.frame
    subdata <- bounds_getdata(paste("data_sub_", ix, ".csv", sep=""))

    # Get regression coefficients for responses under each condition
    subfits <- bounds_fitlm(subdata)
    sub_slope_e <- subfits$re$coefficients[2]
    attributes(sub_slope_e) <- NULL # strip attributes
    sub_slope_d <- subfits$rd$coefficients[2]
    attributes(sub_slope_d) <- NULL
    sub_slope_m <- subfits$rm$coefficients[2]
    attributes(sub_slope_m) <- NULL
    slope_vector_e <- c(slope_vector_e, sub_slope_e) # Concatenate vector
    slope_vector_d <- c(slope_vector_d, sub_slope_d)
    slope_vector_m <- c(slope_vector_m, sub_slope_m)

    # Analyse for decreasing threshold
    prop_dec <- bounds_getpropdec(subdata)
    prop_vector_e <- c(prop_vector_e, prop_dec$easy) # Concatenate vector
    prop_vector_m <- c(prop_vector_m, prop_dec$mix)

    # Calculate average threshold and it's variance
    sub_evidence <- bounds_analyse_evd(subdata)
    evidence_vector_e.mu <- c(evidence_vector_e.mu, sub_evidence$easy.mu)
    evidence_vector_d.mu <- c(evidence_vector_d.mu, sub_evidence$diff.mu)
    evidence_vector_m.mu <- c(evidence_vector_m.mu, sub_evidence$mix.mu)

    # Calculate accurary, reward rate
    sub_perform <- bounds_getacc_rr(subdata)
    acc_vector_e <- c(acc_vector_e, sub_perform$acc_easy) # Concatenate vector
    acc_vector_m <- c(acc_vector_m, sub_perform$acc_mix)
    rr_vector_e <- c(rr_vector_e, sub_perform$rr_easy)
    rr_vector_m <- c(rr_vector_m, sub_perform$rr_mix)

    # Check subject's performance is better than chance
    bounds_btest(subdata, blockno)

    # Calculate reaction time and accuracy at different RTs
    sub_RT <- bounds_getRT(subdata)
    sub_acc_at_RTs <- bounds_getacc_at_RTs(subdata)
    acc_vector_e_fastRT <- c(acc_vector_e_fastRT, sub_acc_at_RTs$e_fast)
    acc_vector_e_slowRT <- c(acc_vector_e_slowRT, sub_acc_at_RTs$e_slow)
    acc_vector_m_fastRT <- c(acc_vector_m_fastRT, sub_acc_at_RTs$m_fast)
    acc_vector_m_slowRT <- c(acc_vector_m_slowRT, sub_acc_at_RTs$m_slow)
    RT_vector_e.mu <- c(RT_vector_e.mu, sub_RT$e.mu)
    RT_vector_d.mu <- c(RT_vector_d.mu, sub_RT$d.mu)
    RT_vector_m_e.mu <- c(RT_vector_m_e.mu, sub_RT$m_e.mu)
    RT_vector_m_d.mu <- c(RT_vector_m_d.mu, sub_RT$m_d.mu)
    RT_vector_e_cor.mu <- c(RT_vector_e_cor.mu, sub_RT$e_cor.mu)
    RT_vector_e_err.mu <- c(RT_vector_e_err.mu, sub_RT$e_err.mu)
    RT_vector_m_e_cor.mu <- c(RT_vector_m_e_cor.mu, sub_RT$m_e_cor.mu)
    RT_vector_m_e_err.mu <- c(RT_vector_m_e_err.mu, sub_RT$m_e_err.mu)

    # Analyse for decreasing threshold
    prop_dec <- bounds_getpropdec(subdata)
    norm_prop_dec.easy <- prop_dec$easy / sub_RT$e.mu # Normalise prop per unit time
    norm_prop_dec.mix <- prop_dec$mix / sub_RT$m.mu
#    prop_vector_e <- c(prop_vector_e, norm_prop_dec.easy) # Concatenate vector
#    prop_vector_m <- c(prop_vector_m, norm_prop_dec.mix)
    prop_vector_e <- c(prop_vector_e, prop_dec$easy) # Concatenate vector
    prop_vector_m <- c(prop_vector_m, prop_dec$mix)

    # Save all variables
    save(list=ls(), file=paste("results_sub_", ix, "_block_", blockno, ".RData", sep=""))

    # Generate a scatterplot of each subjects decisions
    bounds_plotsubdata(subdata)
#    mtext(paste("Sub ", ix, "; Block ", blockno, sep=""), side=3, line=2, adj=1, cex=1)
#    mtext(paste("Slopes: Easy=", round(sub_slope_e, digits=3),
#                "; Diff=", round(sub_slope_d, digits=3),
#                "; Mix=", round(sub_slope_m, digits=3), sep=""),
#                side=3, line=1, adj=1, cex=0.75)
#    mtext(paste("Mean RTs: Easy=", round(sub_RT$e.mu, digits=3),
#                "; Diff=", round(sub_RT$d.mu, digits=3),
#                "; Mix=", round(sub_RT$m.mu, digits=3), sep=""),
#                side=3, line=0, adj=1, cex=0.75)
#    bounds_plot_evdhist(subdata)
    rm(subdata)
}

# Plot proportions graph
bounds_plot_props(prop_vector_e, prop_vector_m)

# Plot Q-Q plot for easy
bounds_qqplot(slope_vector_e, slope_vector_m)

# Plot slopes under each condition
bounds_plot_slopes(slope_vector_e, slope_vector_m)

# Plot relation between reward rate and bounds
#bounds_plot_rrelation(rr_vector_m, slope_vector_m, prop_vector_m, prop_vector_e)

di = slope_vector_m - slope_vector_e
# Partial regression coefficients of RR and Acc wrt slope and threshold
#lmfit_rr_slope_thresh <- lm(rr_vector_m ~ slope_vector_m + evidence_vector_m.mu)
#lmfit_rr_di_thresh <- lm(rr_vector_m ~ di + evidence_vector_m.mu)
#lmfit_rr_dprops_thresh <- lm(rr_vector_m ~ dprops + evidence_vector_m.mu)
#lmfit_slope_thresh_acc <- lm(acc_vector_m ~ slope_vector_m + evidence_vector_m.mu)

# Paired t-test for difference in slopes
sd_di = sd(di)
se_di = sd_di / sqrt(length(di))
Tval_di = mean(di)/se_di
Tval_table = qt(0.975, df=(length(di)-1))
print(paste("mean difference = ", mean(di), sep=""))
print(paste("t-value = ", Tval_di, sep=""))
print(paste("t-value for p<0.05= ", Tval_table, sep=""))

drt_easy = RT_vector_e.mu - RT_vector_m_e.mu
sd_drt_easy = sd(drt_easy)
se_drt_easy = sd_drt_easy / sqrt(length(drt_easy))
Tval_drt_easy = mean(drt_easy) / se_drt_easy
Tval_table_drt_easy = qt(0.975, df=(length(drt_easy)-1))
print("")
print(paste("mean RT_easy - RT_easy_mix = ", mean(drt_easy), sep=""))
print(paste("t-value = ", Tval_drt_easy, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_drt_easy, sep=""))

drt_diff = RT_vector_d.mu - RT_vector_m_d.mu
sd_drt_diff = sd(drt_diff)
se_drt_diff = sd_drt_diff / sqrt(length(drt_diff))
Tval_drt_diff = mean(drt_diff) / se_drt_diff
Tval_table_drt_diff = qt(0.975, df=(length(drt_diff)-1))
print("")
print(paste("mean RT_diff - RT_diff_mix = ", mean(drt_diff), sep=""))
print(paste("t-value = ", Tval_drt_diff, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_drt_diff, sep=""))

# Paired t-test for difference in accuracy for short and long RTs
dacc_easy = acc_vector_e_fastRT - acc_vector_e_slowRT
sd_dacc_easy = sd(dacc_easy)
se_dacc_easy = sd_dacc_easy / sqrt(length(dacc_easy))
Tval_dacc_easy = mean(dacc_easy) / se_dacc_easy
Tval_table_dacc_easy = qt(0.975, df=(length(dacc_easy)-1))
print("")
print(paste("mean(Acc_easy_fastRT_subi - Acc_easy_slowRT_subi) = ", mean(dacc_easy), sep=""))
print(paste("t-value = ", Tval_dacc_easy, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_dacc_easy, sep=""))

dacc_mix = acc_vector_m_fastRT - acc_vector_m_slowRT
sd_dacc_mix = sd(dacc_mix)
se_dacc_mix = sd_dacc_mix / sqrt(length(dacc_mix))
Tval_dacc_mix = mean(dacc_mix) / se_dacc_mix
Tval_table_dacc_mix = qt(0.975, df=(length(dacc_mix)-1))
print("")
print(paste("mean(Acc_mix_fastRT_subi - Acc_mix_slowRT_subi) = ", mean(dacc_mix), sep=""))
print(paste("t-value = ", Tval_dacc_mix, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_dacc_mix, sep=""))

dacc_mix_easy = dacc_mix - dacc_easy
sd_dacc_mix_easy = sd(dacc_mix_easy)
se_dacc_mix_easy = sd_dacc_mix_easy / sqrt(length(dacc_mix_easy))
Tval_dacc_mix_easy = mean(dacc_mix_easy) / se_dacc_mix_easy
Tval_table_dacc_mix_easy = qt(0.975, df=(length(dacc_mix_easy)-1))
print("")
print(paste("mean(Decrease in Acc_mix - Decrease in Acc_easy) = ", mean(dacc_mix_easy), sep=""))
print(paste("t-value = ", Tval_dacc_mix_easy, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_dacc_mix_easy, sep=""))

diff_acc_data <- data.frame("diff_in_acc" = c(dacc_mix, dacc_easy), "condition" = c(rep("mix", nsubs), rep("easy", nsubs)))
boxplot(diff_acc_data$diff_in_acc ~ diff_acc_data$condition, border="black", col="gray", boxwex=0.5)
par(new=TRUE)
abline(0, 0, col="lightgray", lty="dashed")
par(new=TRUE)
boxplot(diff_acc_data$diff_in_acc ~ diff_acc_data$condition, border="black", col="gray", boxwex=0.5, ylab=expression("Accuracy"[fast] - "Accuracy"[slow]))
points(rep(0.7:1.7, each = nsubs), unlist(split(diff_acc_data$diff_in_acc, diff_acc_data$condition)), pch = 16, col="#a96faf")
title(main = "Comparison of accuracies at fast and slow RTs for each S")

# Paired t-test for difference in RTs for correct and error
dRTs_easy_acc = RT_vector_e_err.mu - RT_vector_e_cor.mu
dRTs_mix_acc = RT_vector_m_e_err.mu - RT_vector_m_e_cor.mu
dRTs_cor_err = dRTs_mix_acc - dRTs_easy_acc
sd_dRTs_cor_err = sd(dRTs_cor_err)
se_dRTs_cor_err = sd_dRTs_cor_err / sqrt(length(dRTs_cor_err))
Tval_dRTs_cor_err = mean(dRTs_cor_err) / se_dRTs_cor_err
Tval_table_dRTs_cor_err = qt(0.975, df=(length(dRTs_cor_err)-1))
print("")
print(paste("mean(Decrease in error RTs mix - Decrease in error RTs easy) = ", mean(dRTs_cor_err), sep=""))
print(paste("t-value = ", Tval_dRTs_cor_err, sep=""))
print(paste("t-value for p<0.05= ", Tval_table_dRTs_cor_err, sep=""))

diff_RTs_data <- data.frame("diff_in_RTs" = c(dRTs_easy_acc, dRTs_mix_acc), "condition" = c(rep("easy", nsubs), rep("mix", nsubs)))
boxplot(diff_RTs_data$diff_in_RTs ~ diff_RTs_data$condition, border="black", col="gray", boxwex=0.5)
par(new=TRUE)
abline(0, 0, col="lightgray", lty="dashed")
par(new=TRUE)
boxplot(diff_RTs_data$diff_in_RTs ~ diff_RTs_data$condition, border="black", col="gray", boxwex=0.5, ylab=expression("RTs"[err] - "RTs"[cor]))
points(rep(0.7:1.7, each = nsubs), unlist(split(diff_RTs_data$diff_in_RTs, diff_RTs_data$condition)), pch = 16, col="#a96faf")
title(main = "Comparison of RTs for correct and error trials for each S")

# Plot mean evidence
#bounds_plot_avgevdhist(evidence_vector_e.mu, evidence_vector_d.mu, evidence_vector_m.mu)

# Average performance
print("") # Empty line
print(paste("Average accuracy in easy trials: ", mean(acc_vector_e, sep="")))
print(paste("Average accuracy in mixed trials: ", mean(acc_vector_m, sep="")))
#print(paste("Average latency in easy trials: ", mean(latency_vector_e.mu, sep="")))
#print(paste("Average latency in difficult trials: ", mean(latency_vector_d.mu, sep="")))
#print(paste("Average latency in mixed trials: ", mean(latency_vector_m.mu, sep="")))

save(list=ls(), file=paste("results_allsubs_block_", blockno, ".RData", sep=""))

dev.off()
