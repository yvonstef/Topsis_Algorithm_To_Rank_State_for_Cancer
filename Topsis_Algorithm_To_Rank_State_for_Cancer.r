library(tidyverse)
library(reshape2)

#data
#user should update directory path
BYAREA = read.csv('data/BYAREA.TXT',header=T,sep = "|")

#Collecting list of cancer 
my_list = c("Female Breast","Colon and Rectum","Kidney and Renal Pelvis","Leukemias","Liver and Intrahepatic Bile Duct","Lung and Bronchus","Melanomas of the Skin","Ovary","Prostate","Oral Cavity and Pharynx")

#selecting records with given list of cancer
cancer_data = BYAREA[which(with(BYAREA, EVENT_TYPE == "Incidence" & SITE %in% my_list
                                & RACE == "All Races" & AREA != "Atlanta" & AREA != "Detroit" & AREA != "District of Columbia" 
                                & AREA != "East North Central" & AREA != "East South Central" & AREA != "Los Angeles" 
                                & AREA != "Middle Atlantic" & AREA != "Midwest" & AREA != "Mountain" & AREA != "New England" 
                                & AREA != "Northeast" & AREA != "Pacific" & AREA != "San Francisco-Oakland" & AREA != "San Jose-Monterey" 
                                & AREA != "Seattle-Puget Sound" & AREA != "South" & AREA != "South Atlantic" & AREA != "West" 
                                & AREA != "West North Central" & AREA != "West South Central")),]

#Missing records correction
cancer_data$AGE_ADJUSTED_RATE[cancer_data$AGE_ADJUSTED_RATE == "~"] = NA
cancer_data = na.omit(cancer_data)

#collecting data area, cancer and  Age-Adjusted Rates means
decision_matrix = data.frame("state" = cancer_data$AREA,"site" = cancer_data$SITE,"AGE_ADJUSTED_RATE" = as.numeric(as.character(cancer_data$AGE_ADJUSTED_RATE)))
decision_matrix = acast(decision_matrix, decision_matrix$state ~ decision_matrix$site, mean)


#(1) Calculate the normalized decision matrix.
# All criterion are equally weighted
load = rep(1/10,10) 
sqrt_col_sums = sqrt(colSums(decision_matrix^2))

norm_matrix = matrix(nrow=50, ncol=10)
colnames(norm_matrix) = colnames(decision_matrix)
rownames(norm_matrix)= rownames(decision_matrix)

norm_matrix = matrix(nrow=50, ncol=10)
colnames(norm_matrix) = colnames(decision_matrix)
rownames(norm_matrix)= rownames(decision_matrix)

for (col in 1:ncol(decision_matrix)) {
    norm_matrix[,col] <- (decision_matrix[, col]/sqrt_col_sums[col]) * load[col]
}


#(3) Determine ideal best and ideal worst
optimall_best = apply(norm_matrix, 2, min)
optimal_worst = apply(norm_matrix, 2, max)

#(4) Calculate the separation measures, using the n-dimensional Euclidean distance
distance_worst = function(x) {sqrt( sum( (x - optimal_worst)^2) )}
distance_best = function(x) {sqrt( sum( (x - optimall_best)^2) )}

best = apply(norm_matrix, 1, distance_best)
worst = apply(norm_matrix, 1, distance_worst)

scores = worst/(best + worst)
state_ranks = data.frame(scores = scores, rank = rank(-scores))
state_ranks



