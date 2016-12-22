library(car)
# Initialise variables
subslist <- c(1:24) # All subjects
nsubs <- length(subslist)
pdf(file=paste("results_gono.pdf", sep=""))
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
    confint_m <- confint(logit_m)
    returnlist <- list("easy" = logit_e, "diff" = logit_d, "mix" = logit_m,
                       "ci_e" = confint_e, "ci_d" = confint_d, "ci_m" = confint_m)
    return(returnlist)
}

bounds_plot_coefs_time <- function(arg_coefs_e,arg_coefs_m,
                                   arg_ci_el, arg_ci_eh,
                                   arg_ci_ml, arg_ci_mh)
{
    plot(arg_coefs_e, arg_coefs_m, xlab="Coef_Time (Easy block)",
        ylab="Coef_Time (Mixed block)", xlim=c(-1,2), ylim=c(-1,2), pch=16)
    segments(arg_ci_el, arg_coefs_m, arg_ci_eh, arg_coefs_m, col="gray")
    segments(arg_coefs_e, arg_ci_ml, arg_coefs_e, arg_ci_mh, col="gray")
    #Plot again so that points are on top
    points(arg_coefs_e, arg_coefs_m, pch=16)
    title(main = "Logit coefs of Time in Go ~ Time + Evd", cex=1.5)
    abline(0,1,col="black")
}

bounds_plot_coefs_evid <- function(arg_coefs_e, arg_coefs_m,
                                   arg_ci_el, arg_ci_eh,
                                   arg_ci_ml, arg_ci_mh)
{
    plot(arg_coefs_e, arg_coefs_m, xlab="Coef_Evidence (Easy block)",
        ylab="Coef_Evidence (Mixed block)", xlim=c(-1,2), ylim=c(-1,2), pch=16)
    segments(arg_ci_el, arg_coefs_m, arg_ci_eh, arg_coefs_m, col="gray")
    segments(arg_coefs_e, arg_ci_ml, arg_coefs_e, arg_ci_mh, col="gray")
    #Plot again so that points are on top
    points(arg_coefs_e, arg_coefs_m, pch=16)
    title(main = "Logit coefs of Evidence in Go ~ Time + Evd", cex=1.5)
    abline(0,1)
}

# Gather stats for each subject
for (ix in subslist) {
    # Read raw data from file into data.frame
    subdata <- bounds_getdata(paste("gono_sub_", ix, ".csv", sep=""))

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

# Plot coefficents under each condition
bounds_plot_coefs_time(vector_coef_time_e,
                       vector_coef_time_m,
                       vector_ci_time_el,
                       vector_ci_time_eh,
                       vector_ci_time_ml,
                       vector_ci_time_mh)
bounds_plot_coefs_evid(vector_coef_evid_e,
                       vector_coef_evid_m,
                       vector_ci_evid_el,
                       vector_ci_evid_eh,
                       vector_ci_evid_ml,
                       vector_ci_evid_mh)


save(list=ls(), file=paste("results_allsubs_gono.RData", sep=""))

dev.off()
