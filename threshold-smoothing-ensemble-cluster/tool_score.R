## Tool score
################################################
################################################

# Read in data
toolData.raw <- read.csv('data/raw/ToolScores_8_30_16.csv')
toolData.labels <- toolData.raw[, 1]

# Do groups
#
# Group 1: features 1-5
# Group 2: features 6-12
# Group 3: features 13-17
# Group 4: features 18-21
# Group 5: features 22-25
# Group 6: features 26-29
# + 30th feature = mean score across all features (do not use for factor run)
toolData.nolabels <- toolData.raw[, -1]
tool.group1 <- toolData.nolabels[, 1:5]
tool.group2 <- toolData.nolabels[, 6:12]
tool.group3 <- toolData.nolabels[, 13:17]
tool.group4 <- toolData.nolabels[, 18:21]
tool.group5 <- toolData.nolabels[, 22:25]
tool.group6 <- toolData.nolabels[, 26:29]
#tool.means <- toolData.nolabels[, 30]

tool.noGroup1 <- cbind(tool.group2, tool.group3, tool.group4, tool.group5, tool.group6)
tool.noGroup2 <- cbind(tool.group1, tool.group3, tool.group4, tool.group5, tool.group6)
tool.noGroup3 <- cbind(tool.group1, tool.group2, tool.group4, tool.group5, tool.group6)
tool.noGroup4 <- cbind(tool.group1, tool.group2, tool.group3, tool.group5, tool.group6)
tool.noGroup5 <- cbind(tool.group1, tool.group2, tool.group3, tool.group4, tool.group6)
tool.noGroup6 <- cbind(tool.group1, tool.group2, tool.group3, tool.group4, tool.group5)

tool.all <- cbind(tool.group1, tool.group2, tool.group3, tool.group4, tool.group5, tool.group6)

# Make factor data
toolData.factors.raw <- read.csv('data/raw/ToolScores_8_30_16.csv', colClasses = c(rep('factor', 30)))
toolData.factors <- toolData.factors.raw[, -1]
tool.group1.factor <- toolData.factors[, 1:5]
tool.group2.factor <- toolData.factors[, 6:12]
tool.group3.factor <- toolData.factors[, 13:17]
tool.group4.factor <- toolData.factors[, 18:21]
tool.group5.factor <- toolData.factors[, 22:25]
tool.group6.factor <- toolData.factors[, 26:29]

tool.noGroup1.factor <- cbind(tool.group2.factor, tool.group3.factor, tool.group4.factor, tool.group5.factor, tool.group6.factor)
tool.noGroup2.factor <- cbind(tool.group1.factor, tool.group3.factor, tool.group4.factor, tool.group5.factor, tool.group6.factor)
tool.noGroup3.factor <- cbind(tool.group1.factor, tool.group2.factor, tool.group4.factor, tool.group5.factor, tool.group6.factor)
tool.noGroup4.factor <- cbind(tool.group1.factor, tool.group2.factor, tool.group3.factor, tool.group5.factor, tool.group6.factor)
tool.noGroup5.factor <- cbind(tool.group1.factor, tool.group2.factor, tool.group3.factor, tool.group4.factor, tool.group6.factor)
tool.noGroup6.factor <- cbind(tool.group1.factor, tool.group2.factor, tool.group3.factor, tool.group4.factor, tool.group5.factor)

tool.all.factor <- cbind(tool.group1.factor, tool.group2.factor, tool.group3.factor, tool.group4.factor, tool.group5.factor, tool.group6.factor)

################################################

library(randomUniformForest)
library(GMD)

# Find optimal K based on Elbow
elbow.k <- function(inputDF) {
    #dist.obj <- dist(inputDF)
    dist.obj <- inputDF
    hclust.obj <- hclust(as.dist(inputDF))
    css.obj <- css.hclust(dist.obj, hclust.obj)
    elbow.obj <- elbow.batch(css.obj)
    k <- elbow.obj$k
    return(list(k = k, elbowModel = elbow.obj))
}

#int.noIsl.all.k <- elbow.k(int.noIsl.all.ensemble.mean)
#ext.noIsl.all.k <- elbow.k(ext.noIsl.all.ensemble.mean)
#combo.noIsl.all.k <- elbow.k(combo.noIsl.mean)
#int.noIsl.group1.k <- elbow.k(int.noIsl.group1.ensemble.mean)

################################################
### FACTOR

getEnsembles <- function(fileDir, dataType, subgroup, allLabels, subData, totNum) {
    
    subName <- paste0(dataType, '_', subgroup)
    fullFileName <- paste0(fileDir, subgroup, '/', subName, '_run1.csv')
    unsupRun <- unsupervised.randomUniformForest(subData)
    unsupProx <- unsupRun$proximityMatrix
    colnames(unsupProx) <- allLabels
    rownames(unsupProx) <- allLabels
    write.csv(unsupProx, file = fullFileName)
    
    meanCentersX <- unsupRun$unsupervised$centers[, 1]
    meanCentersY <- unsupRun$unsupervised$centers[, 2]
    betweenSS <- unsupRun$unsupervised$betweenss
    totSS <- unsupRun$unsupervised$totss
    varCenters <- betweenSS / totSS
    
    ensGroups.centerX <- list(meanCentersX)
    ensGroups.centerY <- list(meanCentersY)
    ensGroups.var <- list(varCenters)
    
    ensGroups.list <- list(unsupProx)
    ensGroups.sum <- unsupProx
    
    for (i in 1:totNum) {
        
        subName <- paste0(dataType, '_', subgroup)
        fullFileName <- paste0(fileDir, subgroup, '/', subName, '_run', i, '.csv')
        unsupRun <- unsupervised.randomUniformForest(subData)
        unsupProx <- unsupRun$proximityMatrix
        colnames(unsupProx) <- allLabels
        rownames(unsupProx) <- allLabels
        write.csv(unsupProx, file = fullFileName)
        
        meanCentersX <- unsupRun$unsupervised$centers[, 1]
        meanCentersY <- unsupRun$unsupervised$centers[, 2]
        betweenSS <- unsupRun$unsupervised$betweenss
        totSS <- unsupRun$unsupervised$totss
        varCenters <- betweenSS / totSS
        
        ensGroups.centersX <- c(ensGroups.centerX, list(meanCentersX))
        ensGroups.centersY <- c(ensGroups.centerY, list(meanCentersY))
        ensGroups.var <- c(ensGroups.var, list(varCenters))
        
        ensGroups.list <- c(ensGroups.list, list(unsupProx))
        ensGroups.sum <- ensGroups.sum + unsupProx
    }
    
    ensGroups.mean <- apply(simplify2array(ensGroups.list), 1:2, mean)
    ensGroups.med <- apply(simplify2array(ensGroups.list), 1:2, median)
    ensGroups.sd <- apply(simplify2array(ensGroups.list), 1:2, sd)
    # ensGroups.clusterMean <-
    return(list(ens_list = ensGroups.list, sum = ensGroups.sum, mean = ensGroups.mean, sd = ensGroups.sd, median = ensGroups.med, centerX = ensGroups.centersX, centerY = ensGroups.centersY, centerVar = ensGroups.var))
}



dataType <- 'tools'
allLabels <- toolData.labels
totNum <- 31
fileDir <- 'prox-matrices/factors/'

subgroup <- 'group1'
fact.group1.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group1.factor, totNum)
write.csv(fact.group1.ensemble$mean, file = "prox-matrices/factors/ensemble/group1_mean.csv")
write.csv(fact.group1.ensemble$median, file = "prox-matrices/factors/ensemble/group1_median.csv")
write.csv(fact.group1.ensemble$sd, file = "prox-matrices/factors/ensemble/group1_sd.csv")
# fact.group1.ensemble$sum

group1.varCoverage <- fact.group1.ensemble$centerVar

subgroup <- 'group2'
fact.group2.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group2.factor, totNum)
write.csv(fact.group2.ensemble$mean, file = "prox-matrices/factors/ensemble/group2_mean.csv")
write.csv(fact.group2.ensemble$median, file = "prox-matrices/factors/ensemble/group2_median.csv")
write.csv(fact.group2.ensemble$sd, file = "prox-matrices/factors/ensemble/group2_sd.csv")

group2.varCoverage <- fact.group2.ensemble$centerVar

subgroup <- 'group3'
fact.group3.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group3.factor, totNum)
write.csv(fact.group3.ensemble$mean, file = "prox-matrices/factors/ensemble/group3_mean.csv")
write.csv(fact.group3.ensemble$median, file = "prox-matrices/factors/ensemble/group3_median.csv")
write.csv(fact.group3.ensemble$sd, file = "prox-matrices/factors/ensemble/group3_sd.csv")

group3.varCoverage <- fact.group3.ensemble$centerVar

subgroup <- 'group4'
fact.group4.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group4.factor, totNum)
write.csv(fact.group4.ensemble$mean, file = "prox-matrices/factors/ensemble/group4_mean.csv")
write.csv(fact.group4.ensemble$median, file = "prox-matrices/factors/ensemble/group4_median.csv")
write.csv(fact.group4.ensemble$sd, file = "prox-matrices/factors/ensemble/group4_sd.csv")

group4.varCoverage <- fact.group4.ensemble$centerVar

subgroup <- 'group5'
fact.group5.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group5.factor, totNum)
write.csv(fact.group5.ensemble$mean, file = "prox-matrices/factors/ensemble/group5_mean.csv")
write.csv(fact.group5.ensemble$median, file = "prox-matrices/factors/ensemble/group5_median.csv")
write.csv(fact.group5.ensemble$sd, file = "prox-matrices/factors/ensemble/group5_sd.csv")

group5.varCoverage <- fact.group5.ensemble$centerVar

subgroup <- 'group6'
fact.group6.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.group6.factor, totNum)
write.csv(fact.group6.ensemble$mean, file = "prox-matrices/factors/ensemble/group6_mean.csv")
write.csv(fact.group6.ensemble$median, file = "prox-matrices/factors/ensemble/group6_median.csv")
write.csv(fact.group6.ensemble$sd, file = "prox-matrices/factors/ensemble/group6_sd.csv")

group6.varCoverage <- fact.group6.ensemble$centerVar

subgroup <- 'all'
fact.all.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.all.factor, totNum)
write.csv(fact.all.ensemble$mean, file = "prox-matrices/factors/ensemble/all_mean.csv")
write.csv(fact.all.ensemble$median, file = "prox-matrices/factors/ensemble/all_median.csv")
write.csv(fact.all.ensemble$sd, file = "prox-matrices/factors/ensemble/all_sd.csv")

all.varCoverage <- fact.all.ensemble$centerVar

################################################
# And now leave-one-group-out

### FACTOR

subgroup <- 'noGroup1'
fact.noGroup1.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup1.factor, totNum)
write.csv(fact.noGroup1.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup1_mean.csv")
write.csv(fact.noGroup1.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup1_median.csv")
write.csv(fact.noGroup1.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup1_sd.csv")

noGroup1.varCoverage <- fact.noGroup1.ensemble$centerVar

subgroup <- 'noGroup2'
fact.noGroup2.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup2.factor, totNum)
write.csv(fact.noGroup2.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup2_mean.csv")
write.csv(fact.noGroup2.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup2_median.csv")
write.csv(fact.noGroup2.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup2_sd.csv")

noGroup2.varCoverage <- fact.noGroup2.ensemble$centerVar

subgroup <- 'noGroup3'
fact.noGroup3.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup3.factor, totNum)
write.csv(fact.noGroup3.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup3_mean.csv")
write.csv(fact.noGroup3.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup3_median.csv")
write.csv(fact.noGroup3.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup3_sd.csv")

noGroup3.varCoverage <- fact.noGroup3.ensemble$centerVar

subgroup <- 'noGroup4'
fact.noGroup4.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup4.factor, totNum)
write.csv(fact.noGroup4.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup4_mean.csv")
write.csv(fact.noGroup4.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup4_median.csv")
write.csv(fact.noGroup4.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup4_sd.csv")

noGroup4.varCoverage <- fact.noGroup4.ensemble$centerVar

subgroup <- 'noGroup5'
fact.noGroup5.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup5.factor, totNum)
write.csv(fact.noGroup5.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup5_mean.csv")
write.csv(fact.noGroup5.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup5_median.csv")
write.csv(fact.noGroup5.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup5_sd.csv")

noGroup5.varCoverage <- fact.noGroup5.ensemble$centerVar

subgroup <- 'noGroup6'
fact.noGroup6.ensemble <- getEnsembles(fileDir, dataType, subgroup, allLabels, tool.noGroup6.factor, totNum)
write.csv(fact.noGroup6.ensemble$mean, file = "prox-matrices/factors/ensemble/noGroup6_mean.csv")
write.csv(fact.noGroup6.ensemble$median, file = "prox-matrices/factors/ensemble/noGroup6_median.csv")
write.csv(fact.noGroup6.ensemble$sd, file = "prox-matrices/factors/ensemble/noGroup6_sd.csv")

noGroup6.varCoverage <- fact.noGroup6.ensemble$centerVar

################################################

# K means on results

group1.kmeans.mean <- elbow.k(fact.group1.ensemble$mean)
group1.kmeans.med <- elbow.k(fact.group1.ensemble$median)
group2.kmeans.mean <- elbow.k(fact.group2.ensemble$mean)
group2.kmeans.med <- elbow.k(fact.group2.ensemble$median)
group3.kmeans.mean <- elbow.k(fact.group3.ensemble$mean)
group3.kmeans.med <- elbow.k(fact.group3.ensemble$median)
group4.kmeans.mean <- elbow.k(fact.group4.ensemble$mean)
group4.kmeans.med <- elbow.k(fact.group4.ensemble$median)
group5.kmeans.mean <- elbow.k(fact.group5.ensemble$mean)
group5.kmeans.med <- elbow.k(fact.group5.ensemble$median)
group6.kmeans.mean <- elbow.k(fact.group6.ensemble$mean)
group6.kmeans.med <- elbow.k(fact.group6.ensemble$median)
all.kmeans.mean <- elbow.k(fact.all.ensemble$mean)
all.kmeans.med <- elbow.k(fact.all.ensemble$median)

nogroup1.kmeans.mean <- elbow.k(fact.noGroup1.ensemble$mean)
nogroup1.kmeans.med <- elbow.k(fact.noGroup1.ensemble$median)
nogroup2.kmeans.mean <- elbow.k(fact.noGroup2.ensemble$mean)
nogroup2.kmeans.med <- elbow.k(fact.noGroup2.ensemble$median)
nogroup3.kmeans.mean <- elbow.k(fact.noGroup3.ensemble$mean)
nogroup3.kmeans.med <- elbow.k(fact.noGroup3.ensemble$median)
nogroup4.kmeans.mean <- elbow.k(fact.noGroup4.ensemble$mean)
nogroup4.kmeans.med <- elbow.k(fact.noGroup4.ensemble$median)
nogroup5.kmeans.mean <- elbow.k(fact.noGroup5.ensemble$mean)
nogroup5.kmeans.med <- elbow.k(fact.noGroup5.ensemble$median)
nogroup6.kmeans.mean <- elbow.k(fact.noGroup6.ensemble$mean)
nogroup6.kmeans.med <- elbow.k(fact.noGroup6.ensemble$median)

group1.kmeans.mean.k = group1.kmeans.mean$k
group1.kmeans.mean.elbow = group1.kmeans.mean$elbowModel
group2.kmeans.mean.k = group2.kmeans.mean$k
group2.kmeans.mean.elbow = group2.kmeans.mean$elbowModel
group3.kmeans.mean.k = group3.kmeans.mean$k
group3.kmeans.mean.elbow = group3.kmeans.mean$elbowModel
group4.kmeans.mean.k = group4.kmeans.mean$k
group4.kmeans.mean.elbow = group4.kmeans.mean$elbowModel
group5.kmeans.mean.k = group5.kmeans.mean$k
group5.kmeans.mean.elbow = group5.kmeans.mean$elbowModel
group6.kmeans.mean.k = group6.kmeans.mean$k
group6.kmeans.mean.elbow = group6.kmeans.mean$elbowModel
all.kmeans.mean.k = all.kmeans.mean$k
all.kmeans.mean.elbow = all.kmeans.mean$elbowModel

nogroup1.kmeans.mean.k = nogroup1.kmeans.mean$k
nogroup1.kmeans.mean.elbow = nogroup1.kmeans.mean$elbowModel
nogroup2.kmeans.mean.k = nogroup2.kmeans.mean$k
nogroup2.kmeans.mean.elbow = nogroup2.kmeans.mean$elbowModel
nogroup3.kmeans.mean.k = nogroup3.kmeans.mean$k
nogroup3.kmeans.mean.elbow = nogroup3.kmeans.mean$elbowModel
nogroup4.kmeans.mean.k = nogroup4.kmeans.mean$k
nogroup4.kmeans.mean.elbow = nogroup4.kmeans.mean$elbowModel
nogroup5.kmeans.mean.k = nogroup5.kmeans.mean$k
nogroup5.kmeans.mean.elbow = nogroup5.kmeans.mean$elbowModel
nogroup6.kmeans.mean.k = nogroup6.kmeans.mean$k
nogroup6.kmeans.mean.elbow = nogroup6.kmeans.mean$elbowModel


allK.rowNames <- c("all", "group1", "group2", "group3", "group4", "group5", "group6", "nogroup1", "nogroup2", "nogroup3", "nogroup4", "nogroup5", "nogroup6")
allK.mean.assign <- c(all.kmeans.mean.k, group1.kmeans.mean.k, group2.kmeans.mean.k, group3.kmeans.mean.k, group4.kmeans.mean.k, group5.kmeans.mean.k, group6.kmeans.mean.k, nogroup1.kmeans.mean.k, nogroup2.kmeans.mean.k, nogroup3.kmeans.mean.k, nogroup4.kmeans.mean.k, nogroup5.kmeans.mean.k, nogroup6.kmeans.mean.k)
allK.med.assign <- c(all.kmeans.med.k, group1.kmeans.med.k, group2.kmeans.med.k, group3.kmeans.med.k, group4.kmeans.med.k, group5.kmeans.med.k, group6.kmeans.med.k, nogroup1.kmeans.med.k, nogroup2.kmeans.med.k, nogroup3.kmeans.med.k, nogroup4.kmeans.med.k, nogroup5.kmeans.med.k, nogroup6.kmeans.med.k)

allK.df <- data.frame(data = allK.rowNames, mean_ensemble = allK.mean.assign, median_ensemble = allK.med.assign)

write.csv(allK.df, file = "prox-matrices/factors/final_optimalK.csv")

################################################

# Save out results of optimal K in kmeans 

group1.kmeans.optimalK <- kmeans(fact.group1.ensemble$mean, centers = group1.kmeans.mean.k)
group1.kmeans.optimalK$betweenSS <- group1.kmeans.optimalK$betweenss
group1.kmeans.optimalK$totalSS <- group1.kmeans.optimalK$totss
group1.kmeans.optimalK$clusterVar <- group1.kmeans.optimalK$betweenss / group1.kmeans.optimalK$totss

group2.kmeans.optimalK <- kmeans(fact.group2.ensemble$mean, centers = group2.kmeans.mean.k)
group2.kmeans.optimalK$betweenSS <- group2.kmeans.optimalK$betweenss
group2.kmeans.optimalK$totalSS <- group2.kmeans.optimalK$totss
group2.kmeans.optimalK$clusterVar <- group2.kmeans.optimalK$betweenss / group2.kmeans.optimalK$totss

group3.kmeans.optimalK <- kmeans(fact.group3.ensemble$mean, centers = group3.kmeans.mean.k)
group3.kmeans.optimalK$betweenSS <- group3.kmeans.optimalK$betweenss
group3.kmeans.optimalK$totalSS <- group3.kmeans.optimalK$totss
group3.kmeans.optimalK$clusterVar <- group3.kmeans.optimalK$betweenss / group3.kmeans.optimalK$totss

group4.kmeans.optimalK <- kmeans(fact.group4.ensemble$mean, centers = group4.kmeans.mean.k)
group4.kmeans.optimalK$betweenSS <- group4.kmeans.optimalK$betweenss
group4.kmeans.optimalK$totalSS <- group4.kmeans.optimalK$totss
group4.kmeans.optimalK$clusterVar <- group4.kmeans.optimalK$betweenss / group4.kmeans.optimalK$totss

group5.kmeans.optimalK <- kmeans(fact.group5.ensemble$mean, centers = group5.kmeans.mean.k)
group5.kmeans.optimalK$betweenSS <- group5.kmeans.optimalK$betweenss
group5.kmeans.optimalK$totalSS <- group5.kmeans.optimalK$totss
group5.kmeans.optimalK$clusterVar <- group5.kmeans.optimalK$betweenss / group5.kmeans.optimalK$totss

group6.kmeans.optimalK <- kmeans(fact.group6.ensemble$mean, centers = group6.kmeans.mean.k)
group6.kmeans.optimalK$betweenSS <- group6.kmeans.optimalK$betweenss
group6.kmeans.optimalK$totalSS <- group6.kmeans.optimalK$totss
group6.kmeans.optimalK$clusterVar <- group6.kmeans.optimalK$betweenss / group6.kmeans.optimalK$totss

all.kmeans.optimalK <- kmeans(fact.all.ensemble$mean, centers = all.kmeans.mean.k)
all.kmeans.optimalK$betweenSS <- all.kmeans.optimalK$betweenss
all.kmeans.optimalK$totalSS <- all.kmeans.optimalK$totss
all.kmeans.optimalK$clusterVar <- all.kmeans.optimalK$betweenss / all.kmeans.optimalK$totss

nogroup1.kmeans.optimalK <- kmeans(fact.noGroup1.ensemble$mean, centers = nogroup1.kmeans.mean.k)
nogroup1.kmeans.optimalK$betweenSS <- nogroup1.kmeans.optimalK$betweenss
nogroup1.kmeans.optimalK$totalSS <- nogroup1.kmeans.optimalK$totss
nogroup1.kmeans.optimalK$clusterVar <- nogroup1.kmeans.optimalK$betweenss / nogroup1.kmeans.optimalK$totss

nogroup2.kmeans.optimalK <- kmeans(fact.noGroup2.ensemble$mean, centers = nogroup2.kmeans.mean.k)
nogroup2.kmeans.optimalK$betweenSS <- nogroup2.kmeans.optimalK$betweenss
nogroup2.kmeans.optimalK$totalSS <- nogroup2.kmeans.optimalK$totss
nogroup2.kmeans.optimalK$clusterVar <- nogroup2.kmeans.optimalK$betweenss / nogroup2.kmeans.optimalK$totss

nogroup3.kmeans.optimalK <- kmeans(fact.noGroup3.ensemble$mean, centers = nogroup3.kmeans.mean.k)
nogroup3.kmeans.optimalK$betweenSS <- nogroup3.kmeans.optimalK$betweenss
nogroup3.kmeans.optimalK$totalSS <- nogroup3.kmeans.optimalK$totss
nogroup3.kmeans.optimalK$clusterVar <- nogroup3.kmeans.optimalK$betweenss / nogroup3.kmeans.optimalK$totss

nogroup4.kmeans.optimalK <- kmeans(fact.noGroup4.ensemble$mean, centers = nogroup4.kmeans.mean.k)
nogroup4.kmeans.optimalK$betweenSS <- nogroup4.kmeans.optimalK$betweenss
nogroup4.kmeans.optimalK$totalSS <- nogroup4.kmeans.optimalK$totss
nogroup4.kmeans.optimalK$clusterVar <- nogroup4.kmeans.optimalK$betweenss / nogroup4.kmeans.optimalK$totss

nogroup5.kmeans.optimalK <- kmeans(fact.noGroup5.ensemble$mean, centers = nogroup5.kmeans.mean.k)
nogroup5.kmeans.optimalK$betweenSS <- nogroup5.kmeans.optimalK$betweenss
nogroup5.kmeans.optimalK$totalSS <- nogroup5.kmeans.optimalK$totss
nogroup5.kmeans.optimalK$clusterVar <- nogroup5.kmeans.optimalK$betweenss / nogroup5.kmeans.optimalK$totss

nogroup6.kmeans.optimalK <- kmeans(fact.noGroup6.ensemble$mean, centers = nogroup6.kmeans.mean.k)
nogroup6.kmeans.optimalK$betweenSS <- nogroup6.kmeans.optimalK$betweenss
nogroup6.kmeans.optimalK$totalSS <- nogroup6.kmeans.optimalK$totss
nogroup6.kmeans.optimalK$clusterVar <- nogroup6.kmeans.optimalK$betweenss / nogroup6.kmeans.optimalK$totss

group1.var <- group1.kmeans.optimalK$clusterVar
group2.var <- group2.kmeans.optimalK$clusterVar
group3.var <- group3.kmeans.optimalK$clusterVar
group4.var <- group4.kmeans.optimalK$clusterVar
group5.var <- group5.kmeans.optimalK$clusterVar
group6.var <- group6.kmeans.optimalK$clusterVar
all.var <- all.kmeans.optimalK$clusterVar
nogroup1.var <- nogroup1.kmeans.optimalK$clusterVar
nogroup2.var <- nogroup2.kmeans.optimalK$clusterVar
nogroup3.var <- nogroup3.kmeans.optimalK$clusterVar
nogroup4.var <- nogroup4.kmeans.optimalK$clusterVar
nogroup5.var <- nogroup5.kmeans.optimalK$clusterVar
nogroup6.var <- nogroup6.kmeans.optimalK$clusterVar

allK.rowNames <- c("all", "group1", "group2", "group3", "group4", "group5", "group6", "nogroup1", "nogroup2", "nogroup3", "nogroup4", "nogroup5", "nogroup6")
allK.mean.variance <- c(all.var, group1.var, group2.var, group3.var, group4.var, group5.var, group6.var, nogroup1.var, nogroup2.var, nogroup3.var, nogroup4.var, nogroup5.var, nogroup6.var)

group1.kmeans.optimalK <- kmeans(fact.group1.ensemble$median, centers = group1.kmeans.med.k)
group1.kmeans.optimalK$betweenSS <- group1.kmeans.optimalK$betweenss
group1.kmeans.optimalK$totalSS <- group1.kmeans.optimalK$totss
group1.kmeans.optimalK$clusterVar <- group1.kmeans.optimalK$betweenss / group1.kmeans.optimalK$totss

group2.kmeans.optimalK <- kmeans(fact.group2.ensemble$median, centers = group2.kmeans.med.k)
group2.kmeans.optimalK$betweenSS <- group2.kmeans.optimalK$betweenss
group2.kmeans.optimalK$totalSS <- group2.kmeans.optimalK$totss
group2.kmeans.optimalK$clusterVar <- group2.kmeans.optimalK$betweenss / group2.kmeans.optimalK$totss

group3.kmeans.optimalK <- kmeans(fact.group3.ensemble$median, centers = group3.kmeans.med.k)
group3.kmeans.optimalK$betweenSS <- group3.kmeans.optimalK$betweenss
group3.kmeans.optimalK$totalSS <- group3.kmeans.optimalK$totss
group3.kmeans.optimalK$clusterVar <- group3.kmeans.optimalK$betweenss / group3.kmeans.optimalK$totss

group4.kmeans.optimalK <- kmeans(fact.group4.ensemble$median, centers = group4.kmeans.med.k)
group4.kmeans.optimalK$betweenSS <- group4.kmeans.optimalK$betweenss
group4.kmeans.optimalK$totalSS <- group4.kmeans.optimalK$totss
group4.kmeans.optimalK$clusterVar <- group4.kmeans.optimalK$betweenss / group4.kmeans.optimalK$totss

group5.kmeans.optimalK <- kmeans(fact.group5.ensemble$median, centers = group5.kmeans.med.k)
group5.kmeans.optimalK$betweenSS <- group5.kmeans.optimalK$betweenss
group5.kmeans.optimalK$totalSS <- group5.kmeans.optimalK$totss
group5.kmeans.optimalK$clusterVar <- group5.kmeans.optimalK$betweenss / group5.kmeans.optimalK$totss

group6.kmeans.optimalK <- kmeans(fact.group6.ensemble$median, centers = group6.kmeans.med.k)
group6.kmeans.optimalK$betweenSS <- group6.kmeans.optimalK$betweenss
group6.kmeans.optimalK$totalSS <- group6.kmeans.optimalK$totss
group6.kmeans.optimalK$clusterVar <- group6.kmeans.optimalK$betweenss / group6.kmeans.optimalK$totss

all.kmeans.optimalK <- kmeans(fact.all.ensemble$median, centers = all.kmeans.med.k)
all.kmeans.optimalK$betweenSS <- all.kmeans.optimalK$betweenss
all.kmeans.optimalK$totalSS <- all.kmeans.optimalK$totss
all.kmeans.optimalK$clusterVar <- all.kmeans.optimalK$betweenss / all.kmeans.optimalK$totss

nogroup1.kmeans.optimalK <- kmeans(fact.noGroup1.ensemble$median, centers = nogroup1.kmeans.med.k)
nogroup1.kmeans.optimalK$betweenSS <- nogroup1.kmeans.optimalK$betweenss
nogroup1.kmeans.optimalK$totalSS <- nogroup1.kmeans.optimalK$totss
nogroup1.kmeans.optimalK$clusterVar <- nogroup1.kmeans.optimalK$betweenss / nogroup1.kmeans.optimalK$totss

nogroup2.kmeans.optimalK <- kmeans(fact.noGroup2.ensemble$median, centers = nogroup2.kmeans.med.k)
nogroup2.kmeans.optimalK$betweenSS <- nogroup2.kmeans.optimalK$betweenss
nogroup2.kmeans.optimalK$totalSS <- nogroup2.kmeans.optimalK$totss
nogroup2.kmeans.optimalK$clusterVar <- nogroup2.kmeans.optimalK$betweenss / nogroup2.kmeans.optimalK$totss

nogroup3.kmeans.optimalK <- kmeans(fact.noGroup3.ensemble$median, centers = nogroup3.kmeans.med.k)
nogroup3.kmeans.optimalK$betweenSS <- nogroup3.kmeans.optimalK$betweenss
nogroup3.kmeans.optimalK$totalSS <- nogroup3.kmeans.optimalK$totss
nogroup3.kmeans.optimalK$clusterVar <- nogroup3.kmeans.optimalK$betweenss / nogroup3.kmeans.optimalK$totss

nogroup4.kmeans.optimalK <- kmeans(fact.noGroup4.ensemble$median, centers = nogroup4.kmeans.med.k)
nogroup4.kmeans.optimalK$betweenSS <- nogroup4.kmeans.optimalK$betweenss
nogroup4.kmeans.optimalK$totalSS <- nogroup4.kmeans.optimalK$totss
nogroup4.kmeans.optimalK$clusterVar <- nogroup4.kmeans.optimalK$betweenss / nogroup4.kmeans.optimalK$totss

nogroup5.kmeans.optimalK <- kmeans(fact.noGroup5.ensemble$median, centers = nogroup5.kmeans.med.k)
nogroup5.kmeans.optimalK$betweenSS <- nogroup5.kmeans.optimalK$betweenss
nogroup5.kmeans.optimalK$totalSS <- nogroup5.kmeans.optimalK$totss
nogroup5.kmeans.optimalK$clusterVar <- nogroup5.kmeans.optimalK$betweenss / nogroup5.kmeans.optimalK$totss

nogroup6.kmeans.optimalK <- kmeans(fact.noGroup6.ensemble$median, centers = nogroup6.kmeans.med.k)
nogroup6.kmeans.optimalK$betweenSS <- nogroup6.kmeans.optimalK$betweenss
nogroup6.kmeans.optimalK$totalSS <- nogroup6.kmeans.optimalK$totss
nogroup6.kmeans.optimalK$clusterVar <- nogroup6.kmeans.optimalK$betweenss / nogroup6.kmeans.optimalK$totss

group1.var <- group1.kmeans.optimalK$clusterVar
group2.var <- group2.kmeans.optimalK$clusterVar
group3.var <- group3.kmeans.optimalK$clusterVar
group4.var <- group4.kmeans.optimalK$clusterVar
group5.var <- group5.kmeans.optimalK$clusterVar
group6.var <- group6.kmeans.optimalK$clusterVar
all.var <- all.kmeans.optimalK$clusterVar
nogroup1.var <- nogroup1.kmeans.optimalK$clusterVar
nogroup2.var <- nogroup2.kmeans.optimalK$clusterVar
nogroup3.var <- nogroup3.kmeans.optimalK$clusterVar
nogroup4.var <- nogroup4.kmeans.optimalK$clusterVar
nogroup5.var <- nogroup5.kmeans.optimalK$clusterVar
nogroup6.var <- nogroup6.kmeans.optimalK$clusterVar

allK.med.variance <- c(all.var, group1.var, group2.var, group3.var, group4.var, group5.var, group6.var, nogroup1.var, nogroup2.var, nogroup3.var, nogroup4.var, nogroup5.var, nogroup6.var)

allK.variance.df <- data.frame(data = allK.rowNames, mean_ensemble = allK.mean.variance, median_ensemble = allK.med.variance)

write.csv(allK.variance.df, file = "prox-matrices/factors/final_optimalK_variance.csv")

################################################
# Clusters with optimal K 

group1.kmeans.optimalK$cluster <- group1.kmeans.optimalK$cluster
group2.kmeans.optimalK$cluster <- group2.kmeans.optimalK$cluster
group3.kmeans.optimalK$cluster <- group3.kmeans.optimalK$cluster
group4.kmeans.optimalK$cluster <- group4.kmeans.optimalK$cluster
group5.kmeans.optimalK$cluster <- group5.kmeans.optimalK$cluster
group6.kmeans.optimalK$cluster <- group6.kmeans.optimalK$cluster
all.kmeans.optimalK$cluster <- all.kmeans.optimalK$cluster
nogroup1.kmeans.optimalK$cluster <- nogroup1.kmeans.optimalK$cluster
nogroup2.kmeans.optimalK$cluster <- nogroup2.kmeans.optimalK$cluster
nogroup3.kmeans.optimalK$cluster <- nogroup3.kmeans.optimalK$cluster
nogroup4.kmeans.optimalK$cluster <- nogroup4.kmeans.optimalK$cluster
nogroup5.kmeans.optimalK$cluster <- nogroup5.kmeans.optimalK$cluster
nogroup6.kmeans.optimalK$cluster <- nogroup6.kmeans.optimalK$cluster

group1.clusters.mean <- group1.kmeans.optimalK$cluster
group2.clusters.mean <- group2.kmeans.optimalK$cluster
group3.clusters.mean <- group3.kmeans.optimalK$cluster
group4.clusters.mean <- group4.kmeans.optimalK$cluster
group5.clusters.mean <- group5.kmeans.optimalK$cluster
group6.clusters.mean <- group6.kmeans.optimalK$cluster
all.clusters.mean <- all.kmeans.optimalK$cluster
nogroup1.clusters.mean <- nogroup1.kmeans.optimalK$cluster
nogroup2.clusters.mean <- nogroup2.kmeans.optimalK$cluster
nogroup3.clusters.mean <- nogroup3.kmeans.optimalK$cluster
nogroup4.clusters.mean <- nogroup4.kmeans.optimalK$cluster
nogroup5.clusters.mean <- nogroup5.kmeans.optimalK$cluster
nogroup6.clusters.mean <- nogroup6.kmeans.optimalK$cluster

final.assign.mean <- c(all.clusters.mean, group1.clusters.mean, group2.clusters.mean, group3.clusters.mean, group4.clusters.mean, group5.clusters.mean, group6.clusters.mean, nogroup1.clusters.mean, nogroup2.clusters.mean, nogroup3.clusters.mean, nogroup4.clusters.mean, nogroup5.clusters.mean, nogroup6.clusters.mean)

##### MEDIAN 

group1.kmeans.optimalK$cluster <- group1.kmeans.optimalK$cluster
group2.kmeans.optimalK$cluster <- group2.kmeans.optimalK$cluster
group3.kmeans.optimalK$cluster <- group3.kmeans.optimalK$cluster
group4.kmeans.optimalK$cluster <- group4.kmeans.optimalK$cluster
group5.kmeans.optimalK$cluster <- group5.kmeans.optimalK$cluster
group6.kmeans.optimalK$cluster <- group6.kmeans.optimalK$cluster
all.kmeans.optimalK$cluster <- all.kmeans.optimalK$cluster
nogroup1.kmeans.optimalK$cluster <- nogroup1.kmeans.optimalK$cluster
nogroup2.kmeans.optimalK$cluster <- nogroup2.kmeans.optimalK$cluster
nogroup3.kmeans.optimalK$cluster <- nogroup3.kmeans.optimalK$cluster
nogroup4.kmeans.optimalK$cluster <- nogroup4.kmeans.optimalK$cluster
nogroup5.kmeans.optimalK$cluster <- nogroup5.kmeans.optimalK$cluster
nogroup6.kmeans.optimalK$cluster <- nogroup6.kmeans.optimalK$cluster

group1.clusters.med <- group1.kmeans.optimalK$cluster
group2.clusters.med <- group2.kmeans.optimalK$cluster
group3.clusters.med <- group3.kmeans.optimalK$cluster
group4.clusters.med <- group4.kmeans.optimalK$cluster
group5.clusters.med <- group5.kmeans.optimalK$cluster
group6.clusters.med <- group6.kmeans.optimalK$cluster
all.clusters.med <- all.kmeans.optimalK$cluster
nogroup1.clusters.med <- nogroup1.kmeans.optimalK$cluster
nogroup2.clusters.med <- nogroup2.kmeans.optimalK$cluster
nogroup3.clusters.med <- nogroup3.kmeans.optimalK$cluster
nogroup4.clusters.med <- nogroup4.kmeans.optimalK$cluster
nogroup5.clusters.med <- nogroup5.kmeans.optimalK$cluster
nogroup6.clusters.med <- nogroup6.kmeans.optimalK$cluster

final.assign.med <- c(all.clusters.med, group1.clusters.med, group2.clusters.med, group3.clusters.med, group4.clusters.med, group5.clusters.med, group6.clusters.med, nogroup1.clusters.med, nogroup2.clusters.med, nogroup3.clusters.med, nogroup4.clusters.med, nogroup5.clusters.med, nogroup6.clusters.med)

#final.assign.df <- data.frame(data = allK.rowNames, mean_ensemble = final.assign.mean, median_ensemble = final.assign.med)


# allLabels

final.assign.df <- cbind(all_mean = all.clusters.mean, group1_mean = group1.clusters.mean, group2_mean = group2.clusters.mean, group3_mean = group3.clusters.mean, group4_mean = group4.clusters.mean, group5_mean = group5.clusters.mean, group6_mean = group6.clusters.mean, nogroup1_mean = nogroup1.clusters.mean, nogroup2_mean = nogroup2.clusters.mean, nogroup3_mean = nogroup3.clusters.mean, nogroup4_mean = nogroup4.clusters.mean, nogroup5_mean = nogroup5.clusters.mean, nogroup6_mean = nogroup6.clusters.mean, all_median = all.clusters.med, group1_median = group1.clusters.med, group2_median = group2.clusters.med, group3_median = group3.clusters.med, group4_median = group4.clusters.med, group5_median = group5.clusters.med, group6_median = group6.clusters.med, nogroup1_med = nogroup1.clusters.med, nogroup2_med = nogroup2.clusters.med, nogroup3_med = nogroup3.clusters.med, nogroup4_med = nogroup4.clusters.med, nogroup5_med = nogroup5.clusters.med, nogroup6_med = nogroup6.clusters.med)


write.csv(final.assign.df, file = "prox-matrices/factors/final_cluster_assignments.csv")


###

# Save out ensemble of everything together (single final ensemble)
final.sumEnsemble.coAssign <- (fact.group1.ensemble$mean + fact.group2.ensemble$mean + fact.group3.ensemble$mean + fact.group4.ensemble$mean + fact.group5.ensemble$mean + fact.group6.ensemble$mean + fact.all.ensemble$mean + fact.noGroup1.ensemble$mean + fact.noGroup2.ensemble$mean + fact.noGroup3.ensemble$mean + fact.noGroup4.ensemble$mean + fact.noGroup5.ensemble$mean + fact.noGroup6.ensemble$mean + fact.group1.ensemble$median + fact.group2.ensemble$median + fact.group3.ensemble$median + fact.group4.ensemble$median + fact.group5.ensemble$median + fact.group6.ensemble$median + fact.all.ensemble$median + fact.noGroup1.ensemble$median + fact.noGroup2.ensemble$median + fact.noGroup3.ensemble$median + fact.noGroup4.ensemble$median + fact.noGroup5.ensemble$median + fact.noGroup6.ensemble$median) / 26

final.sumEnsemble.mean <- (fact.group1.ensemble$mean + fact.group2.ensemble$mean + fact.group3.ensemble$mean + fact.group4.ensemble$mean + fact.group5.ensemble$mean + fact.group6.ensemble$mean + fact.all.ensemble$mean + fact.noGroup1.ensemble$mean + fact.noGroup2.ensemble$mean + fact.noGroup3.ensemble$mean + fact.noGroup4.ensemble$mean + fact.noGroup5.ensemble$mean + fact.noGroup6.ensemble$mean) / 13  

final.sumEnsemble.median <- (fact.group1.ensemble$median + fact.group2.ensemble$median + fact.group3.ensemble$median + fact.group4.ensemble$median + fact.group5.ensemble$median + fact.group6.ensemble$median + fact.all.ensemble$median + fact.noGroup1.ensemble$median + fact.noGroup2.ensemble$median + fact.noGroup3.ensemble$median + fact.noGroup4.ensemble$median + fact.noGroup5.ensemble$median + fact.noGroup6.ensemble$median) / 13

final.sumEnsemble.sd <- (fact.group1.ensemble$sd + fact.group2.ensemble$sd + fact.group3.ensemble$sd + fact.group4.ensemble$sd + fact.group5.ensemble$sd + fact.group6.ensemble$sd + fact.all.ensemble$sd + fact.noGroup1.ensemble$sd + fact.noGroup2.ensemble$sd + fact.noGroup3.ensemble$sd + fact.noGroup4.ensemble$sd + fact.noGroup5.ensemble$sd + fact.noGroup6.ensemble$sd) / 13 

write.csv(final.sumEnsemble.coAssign, file = "prox-matrices/factors/final_summary/sum_ensemble_coassignment.csv")
write.csv(final.sumEnsemble.mean, file = "prox-matrices/factors/final_summary/sum_ensemble_mean.csv")
write.csv(final.sumEnsemble.median, file = "prox-matrices/factors/final_summary/sum_ensemble_median.csv")
write.csv(final.sumEnsemble.sd, file = "prox-matrices/factors/final_summary/sum_ensemble_sd.csv")

################################################
################################################
################################################

# Viz: aggregated ensembles + smooth ensembles 
# 
# aggregated - mean similarity matrix of all + subgroup runs 
# smooth - each similarity matrix are projected as cooccurrence, sum together, threshold result

################################################
## Viz AGGREAGTED
################################################

final.aggregate.mean <- final.sumEnsemble.mean
final.aggregate.matrix <- as.matrix(final.aggregate.mean)

### Viz.1 Plot kmeans (with MDS?)

library(cluster)
library(fpc)

final.aggregate.kmeans <- kmeans(final.aggregate.mean, centers = 8)
plotcluster(final.aggregate.matrix, final.aggregate.kmeans$cluster, 
            main = "Kmeans centers of aggregate ensemble")
clusplot(final.aggregate.matrix, final.aggregate.kmeans$cluster, color = TRUE,
            shade = TRUE, labels = 2, lines = 0, 
            main = "Kmeans cluster variance of aggregated ensemble")

aggregate.mds <- cmdscale(final.aggregate.kmeans, eig = TRUE, k = 2)
aggregate.mds.x <- aggregate.mds$points[, 1]
aggregate.mds.y <- aggregate.mds$points[, 2]

### Viz.2 Plot heatmap 

aggregate.heatmap <- heatmap(final.aggregate.matrix, Rowv = NA, Colv = NA, col = cm.colors(256), 
                        scale = "column", margin = c(10, 8), revC = TRUE, 
                        main = "Correlation of aggregate ensemble")

### Viz.3 Plot Dendrograms

final.aggregate.mean.dendrogram <- hclust(as.dist(final.aggregate.mean * -1), method = "average")
#final.dendro.median <- hclust(as.dist(fact.all.ensemble$median * -1), method = "average")
plot(final.aggregate.mean.dendrogram, main = "Mean dendrogram on aggregated ensemble")
#plot(final.dendro.median, main = "Median (all data) dendrogram on final ensemble")

#png(final.aggregate.mean.dendrogram, units = "px", width = 1440, height = 1440)

################################################
## Viz SMOOTH 
################################################

# Bring in co-occurrence matrix 

final.smooth.raw.csv <- read.csv("prox-matrices/factors/final_cluster_coOccur.csv")
tools.rowNames <- final.smooth.raw.csv[, 1]
final.smooth.raw <- final.smooth.raw.csv[, -1]
rownames(final.smooth.raw) <- tools.rowNames

final.smooth.raw.matrix <- as.matrix(final.smooth.raw)

### Viz.1 Plot kmeans (with MDS?)

library(cluster)
library(fpc)

final.smooth.kmeans2 <- kmeans(final.smooth.raw.matrix, centers = 8)

final.smooth.kmeans <- kmeans(final.smooth.raw.matrix, centers = 8)
plotcluster(final.smooth.raw.matrix, final.smooth.kmeans$cluster, 
            main = "Kmeans centers of smooth ensemble")
clusplot(final.smooth.raw.matrix, final.smooth.kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Intra-Cluster Variance in 2-dimensional Space",
         sub = "With 8 centers, these clusters account for 75.6% of the variance.")


# GEE = Google Earth Engine
# NASA = NASA NEX sandbox
# AOD = ArcGIS Open Data
# OSM = OpenStreetMap
# DB = Data Basin
# OGL = OS Geo Live
# AC = AmigoCloud
# GeoODK = OpenDataKit_GeoODK
# AOL = ArcGIS Online
# MG = MapGuide
# GP = Geopaparazzi
# LM = Locus Map
# OM = Orux Maps
# MBS = MapBox Studio
# QGIS = QGIS Cloud
# GL = GeoLocate
# XC = XchangeCore
# ESRI = ESRI Collector for ArcGIS

# GFW = Global Forest Watch
# FP = Field Papers 

newNames <- c("GEE", "NASA", "CARTO", "ROpenSci", "AOD",
              "OSM", "Jupyter", "eBird", "iNaturalist", "DB", "OGL",               
              "Madrona", "Seasketch", "AC", "GeoODK", "Rshiny", "AOL",
              "NextGIS", "MG", "GP", "LM", "OM", "MBS", 
              "QGIS", "GL", "XC", "ESRI", "GFW", "HOLOS", "FP", "FME")
              
newNames2 <- c("T13", "T5", "T1", "T7", "T31",
              "T19", "T4", "T20", "T17", "T23", "T6",               
              "T14", "T29", "T30", "T18", "T8", "T28",
              "T10", "T2", "T25", "T26", "T27", "T15", 
              "T11", "T21", "T3", "T24", "T9", "T22", "T16", "T12")

smooth.clusterss <- final.smooth.0kmeans$cluster
smooth.data <- final.smooth.0raw.matrix

names(smooth.clusterss) <- newNames2
colnames(smooth.data) <- newNames2
rownames(smooth.data) <- newNames2

write.csv(smooth.data, file = "data/smooth_relabeled_data.csv")
write.csv(smooth.clusterss, file = "data/smooth_relabeled_clusters.csv")

smooth.labeled.data <- read.csv('data/smooth_relabeled_data.csv')
smooth.labeled.clusters <- read.csv('data/smooth_relabeled_clusters.csv')

smooth.allLabels <- smooth.labeled.data[, 1]
smooth.labeled.data <- smooth.labeled.data[-1]
smooth.labeled.clusters <- smooth.labeled.clusters[-1]
rownames(smooth.labeled.data) <- smooth.allLabels
rownames(smooth.labeled.clusters) <- smooth.allLabels

#clusplot(final.smooth.0raw.matrix, final.smooth.0kmeans$cluster, color = TRUE,
#         shade = TRUE, labels = 2, lines = 0, 
#         main = "Kmeans cluster variance of smooth ensemble")
#
###



clusplot(smooth.labeled.data, unlist(t(smooth.labeled.clusters)), color = TRUE, cex.txt = 1.25,
         shade = TRUE, labels = 4, lines = 0, plotchar = TRUE, 
         main = "Intra-Cluster Variance in 2-dimensional Space",
         sub = "With 8 centers, these clusters account for 76.3% of the variance.")


#####################################################################################
#####################################################################################
#### SAME THING WITH GROUP1 

#group1.kmeans.optimalK <- kmeans(fact.group1.ensemble$mean, centers = group1.kmeans.mean.k)

smooth.group1.clusterss <- group1.kmeans.optimalK$cluster
smooth.group1.data <- tool.group1
names(smooth.group1.clusterss) <- newNames2
colnames(smooth.group1.data) <- newNames2
rownames(smooth.group1.data) <- newNames2

#smooth.allLabels <- smooth.labeled.data[, 1]
#smooth.labeled.data <- smooth.labeled.data[-1]
#smooth.labeled.clusters <- smooth.labeled.clusters[-1]
#rownames(smooth.labeled.data) <- smooth.allLabels
#rownames(smooth.labeled.clusters) <- smooth.allLabels

clusplot(smooth.group1.data, unlist(t(smooth.group1.clusterss)), color = TRUE, cex.txt = 1.25,
         shade = TRUE, labels = 2, lines = 0, plotchar = TRUE, 
         main = "Intra-Cluster Variance of Group 1 in 2-dimensional Space",
         sub = "With 8 centers, these clusters account for 77.2% of the variance.")


group1.cooccur <- fact.group1.ensemble$mean
colnames(group1.cooccur) <- newNames2 
rownames(group1.cooccur) <- newNames2

my_palette <- colorRampPalette(c("lightblue", "white", "cyan"))(n = 299)

col_breaks = c(seq(0, 25, length = 100),  # for lightblue
               seq(25.00001, 50, length = 100),           # for white
               seq(50.00001, 100, length = 100))             # for cyan

# Put color key under Correlation
lmat = rbind(c(0, 3), c(2, 1), c(0, 4))
lwid = c(0.5, 3)
lhei = c(0.5, 3, 1)

smooth.group1.heatmap <- heatmap.2(as.matrix(group1.cooccur.rd),
                                 cellnote = group1.cooccur.rd,  # same data set for cell labels
                                 main = "Correlation of Tools Group1", # heat map title
                                 notecol = "black",      # change font color of cell labels to black
                                 density.info = "none",  # turns off density plot inside color legend
                                 trace = "none",         # turns off trace lines inside the heat map
                                 #margins = c(12, 8),     # widens margins around plot
                                 col = my_palette,       # use on color palette defined earlier
                                 #col = cm.colors(300),
                                 #col = ,
                                 #breaks = col_breaks,    # enable color transition at specified limits
                                 dendrogram = "none",     # only draw a row dendrogram
                                 Colv = "NA",
                                 Rowv = "NA",
                                 lmat = lmat, lwid = lwid, lhei = lhei) 


#####################################################################################
#####################################################################################
#####################################################################################

### Viz.2 Plot heatmap 

final.smooth.raw.matrix.mod <- final.smooth.raw.matrix 
final.smooth.raw.matrix.mod[final.smooth.raw.matrix.mod == 0] <- 0.0001 
final.smooth.log <- log2(final.smooth.raw.matrix.mod)

smooth.mod <- final.smooth.raw.matrix

colnames(smooth.mod) <- newNames2 
rownames(smooth.mod) <- newNames2

my_palette <- colorRampPalette(c("lightblue", "white", "cyan"))(n = 299)

col_breaks = c(seq(0, 25, length = 100),  # for lightblue
               seq(25.00001, 50, length = 100),           # for white
               seq(50.00001, 100, length = 100))             # for cyan

# Put color key under Correlation
lmat = rbind(c(0, 3), c(2, 1), c(0, 4))
lwid = c(0.75, 3)
lhei = c(0.75, 3, 1)

smooth.mod.heatmap2 <- heatmap.2(as.matrix(smooth.labeled.data),
                                 cellnote = smooth.labeled.data,  # same data set for cell labels
                                 main = "Correlation of Tools", # heat map title
                                 notecol = "black",      # change font color of cell labels to black
                                 density.info = "none",  # turns off density plot inside color legend
                                 trace = "none",         # turns off trace lines inside the heat map
                                 #margins = c(12, 8),     # widens margins around plot
                                 col = my_palette,       # use on color palette defined earlier
                                 #col = cm.colors(300),
                                 #col = ,
                                 #breaks = col_breaks,    # enable color transition at specified limits
                                 dendrogram = "none",     # only draw a row dendrogram
                                 Colv = "NA",
                                 Rowv = "NA",
                                 lmat = lmat, lwid = lwid, lhei = lhei) 
                                


#### Try with GGPLOT2 
#### 

library(RColorBrewer)

my_palette <- colorRampPalette(brewer.pal(9, "Purples"))(100)

library(ggplot2)
library(reshape2)

#smooth.data.cor <- cor(smooth.labeled.data)
smooth.data.cor <- smooth.labeled.data
p <- ggplot(data = smooth.data.cor, aes(x = '', y = '', fill = value)) + geom_tile()

###### LOG SMOOTH 

smooth.log.data <- final.smooth.log
colnames(smooth.log.data) <- newNames 
rownames(smooth.log.data) <- newNames

smooth.log.heatt <- heatmap(smooth.log.data, Rowv = NA, Colv = NA, col = cm.colors(300), 
                            scale = "column", margin = c(10, 8), revC = TRUE, 
                            main = "\nLog Correlation of Tools")

my_palette <- colorRampPalette(c("white", "purple"))(n = 299)

col_breaks = c(seq(0, 33.3333, length = 100),    # for white
               seq(33.3334, 100, length = 100))  # for purple

library(gplots)

# Put color key under Correlation
lmat = rbind(c(0, 3), c(2, 1), c(0, 4))
lwid = c(1.5, 3)
lhei = c(1.5, 3, 1)

smooth.log.heatmap2 <- heatmap.2(smooth.log.data,
                             #cellnote = smooth.log.data,  # same data set for cell labels
                             main = "Log Correlation of Tools", # heat map title
                             notecol = "black",      # change font color of cell labels to black
                             density.info = "none",  # turns off density plot inside color legend
                             trace = "none",         # turns off trace lines inside the heat map
                             #margins = c(12, 8),     # widens margins around plot
                             #col = my_palette,       # use on color palette defined earlier
                             #col = cm.colors(300),
                             #breaks = col_breaks,    # enable color transition at specified limits
                             dendrogram = "none",     # only draw a row dendrogram
                             Colv = "NA",
                             Rowv = "NA")
                             #lmat = lmat, lwid = lwid, lhei = lhei)          

legend("bottom", fill = cm.colors(300), legend = c("-10", "0", "10"))

smooth.heatmap2 

## 

#install.packages("lattice")
library(lattice) 
levelplot(final.smooth.raw.matrix[1:ncol(final.smooth.raw.matrix), ncol(final.smooth.raw.matrix):1])


smooth.heatmap <- heatmap(final.smooth.raw.matrix, Rowx = NA, Colv = NA, col = cm.colors(256), revC = TRUE)
smooth.heatmap

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

col_breaks = c(seq(0, 33.3333, length = 100),  # for red
               seq(33.3334, 66.6666, length = 100),           # for yellow
               seq(66.6667, 100, length = 100))             # for green

smooth.heatmap2 <- heatmap.2(final.smooth.raw.matrix,
                             #cellnote = final.smooth.raw.matrix,  # same data set for cell labels
                             main = "Correlation smooth ensemble", # heat map title
                             notecol = "black",      # change font color of cell labels to black
                             density.info = "none",  # turns off density plot inside color legend
                             trace = "none",         # turns off trace lines inside the heat map
                             margins = c(12, 9),     # widens margins around plot
                             #col = my_palette,       # use on color palette defined earlier
                             #breaks = col_breaks,    # enable color transition at specified limits
                             dendrogram = "row",     # only draw a row dendrogram
                             Colv = "NA")          

smooth.heatmap2 

### Viz.3 Plot Dendrograms

final.smooth.raw.dendrogram <- hclust(as.dist(final.smooth.raw * -1), method = "average")
plot(final.smooth.raw.dendrogram, main = "Dendrogram on smooth ensemble")


################################################
# Get final assignments 
################################################

## Get final cluster assignments 
aggregate.ensemble.clusters <- final.aggregate.kmeans$cluster

final.smooth.raw.csv <- read.csv("prox-matrices/factors/final_cluster_coOccur_mod.csv")
tools.rowNames <- final.smooth.raw.csv[, 1]
final.smooth.raw <- final.smooth.raw.csv[, -1]
rownames(final.smooth.raw) <- tools.rowNames

final.smooth.10raw <- final.smooth.raw
final.smooth.10raw[final.smooth.10raw <= 10] <- 0
final.smooth.10raw.matrix <- as.matrix(final.smooth.10raw)
final.smooth.10raw.elbow <- elbow.k(final.smooth.10raw)
final.smooth.10optimalK <- final.smooth.10raw.elbow$k
final.smooth.10kmeans <- kmeans(final.smooth.10raw.matrix, centers = final.smooth.10optimalK)
clusplot(final.smooth.10raw.matrix, final.smooth.10kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Kmeans cluster variance of smooth ensemble - 10% smooth")

smooth.ensemble.clusters.10perc <- final.smooth.10kmeans$cluster
#ensemble.names <- names(final.aggregate.kmeans$cluster)

final.smooth.25raw <- final.smooth.raw
final.smooth.25raw[final.smooth.25raw <= 25] <- 0
final.smooth.25raw.matrix <- as.matrix(final.smooth.25raw)
final.smooth.25raw.elbow <- elbow.k(final.smooth.25raw)
final.smooth.25optimalK <- final.smooth.25raw.elbow$k
final.smooth.25kmeans <- kmeans(final.smooth.25raw.matrix, centers = final.smooth.25optimalK)
clusplot(final.smooth.25raw.matrix, final.smooth.25kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Kmeans cluster variance of smooth ensemble - 25% smooth")

smooth.ensemble.clusters.25perc <- final.smooth.25kmeans$cluster

final.smooth.33raw <- final.smooth.raw
final.smooth.33raw[final.smooth.33raw <= 33] <- 0
final.smooth.33raw.matrix <- as.matrix(final.smooth.33raw)
final.smooth.33raw.elbow <- elbow.k(final.smooth.33raw)
final.smooth.33optimalK <- final.smooth.33raw.elbow$k
final.smooth.33kmeans <- kmeans(final.smooth.33raw.matrix, centers = final.smooth.33optimalK)
clusplot(final.smooth.33raw.matrix, final.smooth.33kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Kmeans cluster variance of smooth ensemble - 33% smooth")

smooth.ensemble.clusters.33perc <- final.smooth.33kmeans$cluster

final.smooth.50raw <- final.smooth.raw
final.smooth.50raw[final.smooth.50raw <= 50] <- 0
final.smooth.50raw.matrix <- as.matrix(final.smooth.50raw)
final.smooth.50raw.elbow <- elbow.k(final.smooth.50raw)
final.smooth.50optimalK <- final.smooth.50raw.elbow$k
final.smooth.50kmeans <- kmeans(final.smooth.50raw.matrix, centers = final.smooth.50optimalK)
clusplot(final.smooth.50raw.matrix, final.smooth.50kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Kmeans cluster variance of smooth ensemble - 50% smooth")

smooth.ensemble.clusters.50perc <- final.smooth.50kmeans$cluster

final.smooth.0raw <- final.smooth.raw
final.smooth.0raw.matrix <- as.matrix(final.smooth.0raw)
final.smooth.0raw.elbow <- elbow.k(final.smooth.0raw)
final.smooth.0optimalK <- final.smooth.0raw.elbow$k
final.smooth.0kmeans <- kmeans(final.smooth.0raw.matrix, centers = final.smooth.0optimalK)
clusplot(final.smooth.0raw.matrix, final.smooth.0kmeans$cluster, color = TRUE,
         shade = TRUE, labels = 2, lines = 0, 
         main = "Kmeans cluster variance of smooth ensemble")

smooth.ensemble.clusters.0perc <- final.smooth.0kmeans$cluster

final.assignments <- data.frame(aggregate = aggregate.ensemble.clusters,
                                smooth = smooth.ensemble.clusters.0perc,
                                smooth_10perc = smooth.ensemble.clusters.10perc,
                                smooth_25perc = smooth.ensemble.clusters.25perc, 
                                smooth_33perc = smooth.ensemble.clusters.33perc,
                                smooth_50perc = smooth.ensemble.clusters.50perc)


write.csv(final.assignments, file = "prox-matrices/cluster_assignments_sept12.csv")

################################################
################################################
################################################

## Make threshold line plot for Figure, part 2

Value <- c(1:100)
yy <- c(1:100) # create some data 
yy[yy < 65] = 0

par(pch = 22, col = "black") # plotting symbol and color 
#par(mfrow = c(2,4)) # all plots on one page 
#opts = c("p","l","o","b","c","s","S","h") 
#for(i in 1:length(opts)){ 
#    heading = paste("type=",opts[i]) 
#    plot(x, y, type="n", main=heading) 
#    lines(x, y, type=opts[i]) 
#}

plot(Value, yy, ylab = "Dropout Amount", type = "n", main = "Threshold Smoothing")
lines(Value, yy)

################################################
################################################
################################################
