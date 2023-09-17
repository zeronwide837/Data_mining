setwd("작업 폴더 주소")

##Data Processing
library(arules)
library(arulesViz)

BNKSERV = read.transactions("BNKSERV.csv", format = "single", cols = c(1,2), sep=",",skip=1,rm.duplicate=TRUE)
str(BNKSERV)
as(BNKSERV, "data.frame")[1:10,]


##Checking Support Factor
suprule = apriori(BNKSERV, parameter = list(support = 0.1, minlen = 2), control=list(verbose=F))
suprule.sorted = sort(suprule, by = "support")
inspect(suprule.sorted)


##Checking Confidence Factor
confirule = apriori(BNKSERV, parameter = list(confidence = 0.6, minlen = 2), control=list(verbose=F))
confirule.sorted = sort(confirule, by = "confidence")
inspect(confirule.sorted)


##Association Analysis
rules = apriori(BNKSERV, parameter=list(support=0.1, confidence=0.6, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by = c("support","lift"))
inspect(rules.sorted)

#subrule1 : rhs should include SVG
rules.sub1 = subset(rules, subset = rhs %in% "SVG" & lift > 1)
inspect(rules.sub1)

#subrule2 : rhs should include CKING
rules.sub2 = subset(rules, subset = rhs %in% "CKING" & lift > 1)
inspect(rules.sub2)

#subrule3 : lhs should include ATM
rules.sub3 = subset(rules, subset = lhs %in% "ATM" & lift > 1)
inspect(rules.sub3)


##Visualization
plot(rules)
plot(rules, measure = c("support","lift"),shading="confidence")

#lower cutline to check association more easily
Nrules = apriori(BNKSERV, parameter=list(support=0.03, confidence=0.4, minlen=2), control=list(verbose=F))
plot(Nrules)
