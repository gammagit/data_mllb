library(car)
COMPARE_EASY <- 0 # Boolean indicates if comparison is easy-mix or diff-mix
# Initialise variables
subslist <- c(1:24) # All subjects
nsubs <- length(subslist)
pdf(file=paste("results_gono_exp2.pdf", sep=""))
vector_coef_time_e <- NULL
vector_coef_time_d <- NULL
vector_coef_time_m <- NULL
vector_coef_evid_e <- NULL
vector_coef_evid_d <- NULL
vector_coef_evid_m <- NULL
vector_ci_time_el <- NULL
vector_ci_time_eh <- NULL
vector_ci_time_dl <- NULL
vector_ci_time_dh <- NULL
vector_ci_time_ml <- NULL
vector_ci_time_mh <- NULL
vector_ci_evid_el <- NULL
vector_ci_evid_eh <- NULL
vector_ci_evid_dl <- NULL
vector_ci_evid_dh <- NULL
vector_ci_evid_ml <- NULL
vector_ci_evid_mh <- NULL

bounds_getdata <- function(arg_filename)
{
    subtable <- read.csv(arg_filename, header=TRUE)
    subdata <- data.frame(subtable)
    return(subdata)
}

bounds_fit_logit <- function(arg_data)
{
    # Fit logistic regression to Go/Wait actions
    logit_e <- glm(action[condition==1] ~
                   time[condition==1] +
                   evidence[condition==1],
                   data=arg_data,
                   family="binomial")
    confint_e <- confint(logit_e)
    logit_d <- glm(action[condition==2] ~
                   time[condition==2] +
                   evidence[condition==2],
                   data=arg_data,
                   family="binomial")
    confint_d <- confint(logit_d)
    logit_m <- glm(action[condition==3] ~
                   time[condition==3] +
                   evidence[condition==3],
                   data=arg_data,
                   family="binomial")
    ### Uncomment for Easy-Easy comparison
#    logit_m <- glm(action[condition==3 & (coherence==0.90|coherence==0.10)] ~
#                   time[condition==3 & (coherence==0.90|coherence==0.10)] +
#                   evidence[condition==3 & (coherence==0.90|coherence==0.10)],
#                   data=arg_data,
#                   family="binomial")
    confint_m <- confint(logit_m)
    returnlist <- list("easy" = logit_e, "diff" = logit_d, "mix" = logit_m,
                       "ci_e" = confint_e, "ci_d" = confint_d, "ci_m" = confint_m)
    return(returnlist)
}

bounds_plot_coefs_time <- function(arg_easy,
                                   arg_coefs_1,arg_coefs_2,
                                   arg_ci_1l, arg_ci_1h,
                                   arg_ci_2l, arg_ci_2h)
{
    if (arg_easy == 1) {
        xlab_string <- "Coef_Time (Easy block)"
    }
    else {
        xlab_string <- "Coef_Time (Diff block)"
    }
    ylab_string = "Coef_Time (Mixed block)"
    plot(arg_coefs_1, arg_coefs_2, xlab=xlab_string,
        ylab=ylab_string, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), pch=16)
    abline(h=0,v=0,lty=2)
    segments(arg_ci_1l, arg_coefs_2, arg_ci_1h, arg_coefs_2, col="gray85")
    segments(arg_coefs_1, arg_ci_2l, arg_coefs_1, arg_ci_2h, col="gray85")
    #Plot again so that points are on top
    points(arg_coefs_1, arg_coefs_2, pch=16, col="tomato")
    text(arg_coefs_1, arg_coefs_2, labels=subslist, cex=0.4)
    title(main = "Logit coefs of Time in Go ~ Time + Evd", cex=1.5)
    abline(0,1,col="black")
}

bounds_plot_coefs_evid <- function(arg_easy,
                                   arg_coefs_1, arg_coefs_2,
                                   arg_ci_1l, arg_ci_1h,
                                   arg_ci_2l, arg_ci_2h)
{
    if (arg_easy == 1) {
        xlab_string <- "Coef_Evidence (Easy block)"
    }
    else {
        xlab_string <- "Coef_Evidence (Diff block)"
    }
    ylab_string = "Coef_Evidence (Mixed block)"
    plot(arg_coefs_1, arg_coefs_2, xlab=xlab_string,
        ylab=ylab_string, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), pch=16)
    abline(h=0,v=0,lty=2)
    segments(arg_ci_1l, arg_coefs_2, arg_ci_1h, arg_coefs_2, col="gray85")
    segments(arg_coefs_1, arg_ci_2l, arg_coefs_1, arg_ci_2h, col="gray85")
    #Plot again so that points are on top
    points(arg_coefs_1, arg_coefs_2, pch=16, col="tomato")
    text(arg_coefs_1, arg_coefs_2, labels=subslist, cex=0.4)
    title(main = "Logit coefs of Evidence in Go ~ Time + Evd", cex=1.5)
    abline(0,1)
}

bounds_plot_slopes <- function(arg_easy, arg_slopes_1, arg_slopes_2)
{
    top <- 2
    bottom <- -3

    if (arg_easy == 1) {
        xlab_string <- "Slopes (Easy block)"
    }
    else {
        xlab_string <- "Slopes (Diff block)"
    }
    ylab_string = "Slopes (Mixed block)"

    trunc_slopes = bounds_trunc_slopes(arg_slopes_1, arg_slopes_2, top, bottom)

#    plot(arg_slopes_1, arg_slopes_2, xlab=xlab_string,
    plot(trunc_slopes$one, trunc_slopes$two, xlab=xlab_string,
#        ylab=ylab_string, pch=16, col="tomato")
        ylab=ylab_string, xlim=c(bottom,top), ylim=c(bottom,top), pch=16, col="tomato")
#    text(arg_slopes_1, arg_slopes_2, labels=subslist, cex=0.4)
    text(trunc_slopes$one, trunc_slopes$two, labels=subslist, cex=0.4)
    title(main = "Slopes of P(Go) = P(Wait) for all subjects", cex=1.5)
    abline(h=0,v=0,lty=2)
    abline(0,1)
}

bounds_trunc_slopes <- function(arg_slopes_1, arg_slopes_2, arg_top, arg_bottom)
# Truncate graph to [bottom, top] for display
{
    out_slopes_1 <- arg_slopes_1
    out_slopes_2 <- arg_slopes_2

    out_slopes_1[which(out_slopes_1 > arg_top)] <- arg_top
    out_slopes_1[which(out_slopes_1 < arg_bottom)] <- arg_bottom
    out_slopes_2[which(out_slopes_2 > arg_top)] <- arg_top
    out_slopes_2[which(out_slopes_2 < arg_bottom)] <- arg_bottom

    return(list("one"=out_slopes_1, "two"=out_slopes_2))
}

# Gather stats for each subject
for (ix in subslist) {
    # Read raw data from file into data.frame
    subdata <- bounds_getdata(paste("gono_sub_", ix, ".csv", sep=""))
    print(ix)

    logit_fits <- bounds_fit_logit(subdata)

    coef_time_e <- logit_fits$easy$coefficients[2]
    coef_evid_e <- logit_fits$easy$coefficients[3]
    ci_time_el <- logit_fits$ci_e[2,1] # Very clumsy!!!
    ci_time_eh <- logit_fits$ci_e[2,2]
    ci_evid_el <- logit_fits$ci_e[3,1]
    ci_evid_eh <- logit_fits$ci_e[3,2]
    attributes(coef_time_e) <- NULL # strip attributes
    attributes(coef_evid_e) <- NULL # strip attributes

    coef_time_d <- logit_fits$diff$coefficients[2]
    coef_evid_d <- logit_fits$diff$coefficients[3]
    ci_time_dl <- logit_fits$ci_d[2,1] # Very clumsy!!!
    ci_time_dh <- logit_fits$ci_d[2,2]
    ci_evid_dl <- logit_fits$ci_d[3,1]
    ci_evid_dh <- logit_fits$ci_d[3,2]
    attributes(coef_time_d) <- NULL # strip attributes
    attributes(coef_evid_d) <- NULL # strip attributes

    coef_time_m <- logit_fits$mix$coefficients[2]
    coef_evid_m <- logit_fits$mix$coefficients[3]
    ci_time_ml <- logit_fits$ci_m[2,1] # Very clumsy!!!
    ci_time_mh <- logit_fits$ci_m[2,2]
    ci_evid_ml <- logit_fits$ci_m[3,1]
    ci_evid_mh <- logit_fits$ci_m[3,2]
    attributes(coef_time_m) <- NULL # strip attributes
    attributes(coef_evid_m) <- NULL # strip attributes

    vector_coef_time_e <- c(vector_coef_time_e, coef_time_e) # Concatenate vector
    vector_coef_time_d <- c(vector_coef_time_d, coef_time_d)
    vector_coef_time_m <- c(vector_coef_time_m, coef_time_m)
    vector_ci_time_el <- c(vector_ci_time_el, ci_time_el)
    vector_ci_time_eh <- c(vector_ci_time_eh, ci_time_eh)
    vector_ci_time_dl <- c(vector_ci_time_dl, ci_time_dl)
    vector_ci_time_dh <- c(vector_ci_time_dh, ci_time_dh)
    vector_ci_time_ml <- c(vector_ci_time_ml, ci_time_ml)
    vector_ci_time_mh <- c(vector_ci_time_mh, ci_time_mh)

    vector_coef_evid_e <- c(vector_coef_evid_e, coef_evid_e)
    vector_coef_evid_d <- c(vector_coef_evid_d, coef_evid_d)
    vector_coef_evid_m <- c(vector_coef_evid_m, coef_evid_m)
    vector_ci_evid_el <- c(vector_ci_evid_el, ci_evid_el)
    vector_ci_evid_eh <- c(vector_ci_evid_eh, ci_evid_eh)
    vector_ci_evid_dl <- c(vector_ci_evid_dl, ci_evid_dl)
    vector_ci_evid_dh <- c(vector_ci_evid_dh, ci_evid_dh)
    vector_ci_evid_ml <- c(vector_ci_evid_ml, ci_evid_ml)
    vector_ci_evid_mh <- c(vector_ci_evid_mh, ci_evid_mh)

    rm(subdata)
}

slopes_e <- (-1) * vector_coef_time_e / vector_coef_evid_e
slopes_d <- (-1) * vector_coef_time_d / vector_coef_evid_d
slopes_m <- (-1) * vector_coef_time_m / vector_coef_evid_m

# Plot coefficents under each condition
if (COMPARE_EASY == 1) {
    bounds_plot_coefs_time(1,
                        vector_coef_time_e,
                        vector_coef_time_m,
                        vector_ci_time_el,
                        vector_ci_time_eh,
                        vector_ci_time_ml,
                        vector_ci_time_mh)
    bounds_plot_coefs_evid(1,
                        vector_coef_evid_e,
                        vector_coef_evid_m,
                        vector_ci_evid_el,
                        vector_ci_evid_eh,
                        vector_ci_evid_ml,
                        vector_ci_evid_mh)
    bounds_plot_slopes(1, slopes_e, slopes_m)
    di = slopes_e - slopes_m
} else {
    bounds_plot_coefs_time(0,
                        vector_coef_time_d,
                        vector_coef_time_m,
                        vector_ci_time_dl,
                        vector_ci_time_dh,
                        vector_ci_time_ml,
                        vector_ci_time_mh)
    bounds_plot_coefs_evid(0,
                        vector_coef_evid_d,
                        vector_coef_evid_m,
                        vector_ci_evid_dl,
                        vector_ci_evid_dh,
                        vector_ci_evid_ml,
                        vector_ci_evid_mh)
    bounds_plot_slopes(0, slopes_d, slopes_m)
    di = slopes_d - slopes_m
}

easy_test <- t.test(slopes_e, mu=0)
print(easy_test)
diff_test <- t.test(slopes_d, mu=0)
print(diff_test)
mix_test <- t.test(slopes_m, mu=0)
print(mix_test)

em_test <- t.test(slopes_d, slopes_m, paired=TRUE)
print(em_test)


# Paired t-test for difference in slopes
sd_di = sd(di)
se_di = sd_di / sqrt(length(di))
Tval_di = mean(di)/se_di
Tval_table = qt(0.975, df=(length(di)-1))
print(paste("mean difference = ", mean(di), sep=""))
print(paste("t-value = ", Tval_di, sep=""))
print(paste("t-value for p<0.05= ", Tval_table, sep=""))

save(list=ls(), file=paste("results_allsubs_gono.RData", sep=""))

dev.off()
