library(plyr)

source("/Users/karen2/latex/papers/dust/steph_diss/analysis/2_load_data_for_prob_models.R")

# Category marginalization experiments. IE, can we easily margenalize out Attributes

categs.occured[which(categs.occured$Class=="animalhair" & categs.occured$Subclass=="cat"),]
X.pop[,which(categs.occured$Class=="animalhair" & categs.occured$Subclass=="cat")]
# Anyone  != 0 had cat hair
rowSums(X[,which(categs.occured$Class=="animalhair" & categs.occured$Subclass=="cat")])
# Anyone  == 1 had cat hair
as.numeric(rowSums(X[,which(categs.occured$Class=="animalhair" & categs.occured$Subclass=="cat")]) != 0)

categs.occured[,c(2,3)]
row.match(c("various", "twig"), categs.occured[,c(2,3)])
row.match(c("animalhair", "cat"), categs.occured[,c(2,3)]) # Only returns first row found...... UGH
row.match(c("animalhair", "dog"), categs.occured[,c(2,3)]) # Only returns first row found...... UGH
categs.occured[,c(2,3)]

colnames(data.frame(categs.occured[,c(2,3)]))
svj <- data.frame(array(c("animalhair", "dog"), c(1,2)))
svj
colnames(svj) <- c("Class", "Subclass")
match_df(data.frame(categs.occured[,c(2,3)]), svj)
as.numeric(rownames(match_df(data.frame(categs.occured[,c(2,3)]), svj)))

data.frame(array(c("animalhair", "dog"), c(1,2)))

# IS THERE A BETTER WAY TO DO THIS WITH gRbase functions??!!

margenalize_to(a.class = "various", a.subclass = "twig", categmat = categs.occured, datamat = X)

margenalize_to(a.class = "animalhair", an.attribute = "red", categmat = categs.occured, datamat = X)
margenalize_to(a.subclass = "dog", an.attribute = "red", categmat = categs.occured, datamat = X)
margenalize_to(a.subclass = "cat", an.attribute = "red", categmat = categs.occured, datamat = X)
margenalize_to(an.attribute = "red", a.subclass = "dog", categmat = categs.occured, datamat = X)
margenalize_to(a.class = "animalhair", a.subclass = "cat", categmat = categs.occured, datamat = X)
margenalize_to(a.class = "naturalfibers", a.subclass = "wool", categmat = categs.occured, datamat = X)

junk <- margenalize_to(a.class = "naturalfibers", categmat = categs.occured, datamat = X)
margenalize_to(a.subclass = "cat", categmat = categs.occured, datamat = X)
margenalize_to(an.attribute = "red", categmat = categs.occured, datamat = X)

# dplyr slice views??
# MRF between clusters of nodes
