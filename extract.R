# extract file names
sim_filenames <- list.files(path = "./out")
N <- as.numeric(sapply(sim_filenames, function(x) substr(x, 9, 10)))
term <- rep(NA, length(sim_filenames)) 

# example summary statistic
success_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  success_Ds[i] <- mean(tab$countsumpopsuccessDs)
}

SDsuccess_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  SDsuccess_Ds[i] <- sd(tab$countsumpopsuccessDs)
}


success_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  success_F[i] <- mean(tab$countsumpopsuccessF)
}

SDsuccess_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  SDsuccess_F[i] <- sd(tab$countsumpopsuccessF)
}



# get sum of male reprod
sum_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  sum_Ds[i] <- mean(tab$countsumpopsuccessDs+tab$countsumpopfailureDs)
}

SDsum_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  SDsum_Ds[i] <- sd(tab$countsumpopsuccessDs+tab$countsumpopfailureDs)
}


# get sum of female reprod
sum_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  sum_F[i] <- mean(tab$countsumpopsuccessF+tab$countsumpopfailureF)
}

SDsum_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  SDsum_F[i] <- sd(tab$countsumpopsuccessF+tab$countsumpopfailureF)
}

# Get proportion of male and female repro success
# Not successful

propsuccess_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  total_Ds <- (tab$countsumpopsuccessDs+tab$countsumpopfailureDs)
  propsuccess_Ds[i] <- mean(tab$countsumpopsuccessDs/total_Ds)
}

SDpropsuccess_Ds <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  total_Ds <- (tab$countsumpopsuccessDs+tab$countsumpopfailureDs)
  SDpropsuccess_Ds[i] <- sd(tab$countsumpopsuccessDs/total_Ds)
}

propsuccess_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  total_F <- (tab$countsumpopsuccessF+tab$countsumpopfailureF)
  propsuccess_F[i] <- mean(tab$countsumpopsuccessF/total_F)
}

SDpropsuccess_F <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  total_F <- (tab$countsumpopsuccessF+tab$countsumpopfailureF)
  SDpropsuccess_F[i] <- sd(tab$countsumpopsuccessF/total_F)
}


# attempt to get number of rows
termcomp <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  termcomp[i] <- ifelse(term[i]>=52178,1,0)
}

termincomp <- rep(NA, length(sim_filenames))
for(i in 1:length(sim_filenames)) {
  tab <- read.csv(paste0("./out/", sim_filenames[i]), header = TRUE)
  term[i] <- nrow(tab)
  termincomp[i] <- ifelse(term[i]<52178,1,0)
}

# Save to dataframe csv
runsummary<- data.frame(N, term, 
                        success_Ds, SDsuccess_Ds,
                        sum_Ds, SDsum_Ds,
                        success_F, SDsuccess_F,
                        sum_F, SDsum_F,
                        termcomp, termincomp)

write.csv(runsummary, "runsummaryweeklytry8r2.csv", row.names = FALSE)



# Graphs
plot(term ~ N, ylab = "No. of runs bef. termination")

# trying to get the number of success and failure term 1000 years 
# for all the runs done at a certain pop size N
# How to count number of run for a pop size N?
plot(termcomp/(termcomp+termincomp) ~ N, ylab = "Prop. of runs complete at termination")

plot(termincomp/(termcomp+termincomp) ~ N, ylab = "Prop. of runs incomplete at termination")

# may not be meaningful
#plot(success_Ds/N ~ N, ylab = "Prop. male success per pop")

# may not be meaningful
#plot(success_F/N ~ N, ylab = "Prop. female success pop")

# How to get the mean for each pop size N?
plot(success_Ds/sumsuccess_Ds ~ N, ylab = "Prop. male success per reprod")

plot(success_F/sumsuccess_F ~ N, ylab = "Prop. female success per reprod")

