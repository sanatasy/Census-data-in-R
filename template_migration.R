load("C:\\Users\\Sanata\\Dropbox\\08 Migration Paper\\Datasets\\01 Censuses\\cameroon_census05_edited.RData")

##R file for migration paper 
##Started 09/2015 
##Uses IPUMS census data 
##This is the template file, which uses Zambia as example 

#### Packages ##### 
require(foreign)
library(readstata13)
####################

###### Header ######
saveloc <- "C:\\Users\\Sanata\\Dropbox\\08 Migration Paper\\Datasets\\"
seed <- "C:\\Users\\Sanata\\Dropbox\\08 Migration Paper\\IPUMS Migration\\"
location <- paste0(seed, "Zambia\\")
setwd(location)

country <- "zambia"
cname <- "Zambia"
years <- c("90","00", "10")
####################



####### Read file ############
year <- years[3]
zam <- read.dta13( paste0(location, country, "_census", year, ".dta"), convert.factors = F, 
                   convert.underscore = T)

#ID Household Head
zam$hhead <- ifelse(zam$relate==1, 1, 0)
#zam <- zam[zam$hhead==1, ]


############### DATA CLEANING ##################

str(zam)
#get var names 
colnames(zam)
#get variable descriptions 
attributes(zam)$var.labels
#get list of label names for all variables
attributes(zam)$val.labels

#assign current location units 
zam$lev1 <-zam$geo1b.zm
zam$lev2 <- zam$geo2b.zm 
zam$unit <- zam$geo2b.zm
zam$migunit <- zam$migzm

#assign birthplace location units 
zam$bpl <- zam$bplzm
zam$bpl.lev1 <- as.numeric(with(zam, 
                     ifelse(nchar(as.character(bpl))==4, 
                            substr(as.character(bpl), 1, 2), 
                            substr(as.character(bpl), 1, 1)) 
))
table(zam$bpl.lev1)
zam$bpl.lev2 <- zam$bplzm

# OPTIONAL: Fix unit assignments here 
zam$lev1 <- ifelse(zam$geo2b.zm>=601 & zam$geo2b.zm<=606, 
                   6, 
                   zam$geo1b.zm)

#####################################



################# BEGIN CODE HERE ######################

########################################################


################# CREATE INDICATORS #####################

#new indicators 
#Moved from birthplace level 1 (region) 
zam$movedbpl.lev1 <- ifelse(zam$bpl.lev1 != zam$lev1, 1, 0)
#Moved from birthplace level 2 (district)
zam$movedbpl.lev2 <- ifelse(zam$bpl.lev2 != zam$lev2, 1, 0)
#Intra-regional move from birthplace unit 
zam$intrareg.bplmove <- ifelse(zam$bpl.lev2 != zam$lev2 & zam$bpl.lev1 == zam$lev1, 1, 0)
#Moved locality in past 5 years or less 
zam$mg5 <- ifelse(zam$mgyrs1<=5 | zam$mgyrs1==96 , 1, 0)

#country-level proportions 
p.moved.bpl.reg <- prop.table(table(zam$movedbpl.lev1))[2]
p.moved.bpl.dist <- prop.table(table(zam$movedbpl.lev2))[2]
p.intrareg.bplmove <- prop.table(table(zam$intrareg.bplmove))[2]
#Inter-reg AMONG birthplace movers 
temp <- zam[zam$movedbpl.lev2==1, ]
p.interreg.of.bplmovers  <- 1- prop.table(table(temp$intrareg.bplmove))[2]
p.move.last5 <- prop.table(table(zam$mg5))[2]

country.stats <- as.data.frame(cbind(p.moved.bpl.reg, p.moved.bpl.dist, p.intrareg.bplmove, 
                       p.interreg.of.bplmovers, p.move.last5))
country.stats$country <- cname
country.stats$year <- year

save(country.stats, file=paste0(saveloc, "02 Country Stats\\", country, year, "_countrystats_indiv.RData"))
#save(country.stats, file=paste0(saveloc, "02 Country Stats\\", country, year, "_countrystats_indiv.RData"))

save(zam, file=paste0(saveloc, "01 Censuses\\", country, "_census", year, "_edited.RData"))


################# CALC PROPORTIONS ######################

###By Current unit 

#sample prop of those in current unit that have a different birthplace level 1 (region)
p <- with(zam, tapply(serial, list(movedbpl.lev1, unit), length))
p.out.bpl.region <- prop.table(p,2)[2,]

#sample prop of those in current unit that have a different birthplace level 2 (district)
p <- with(zam, tapply(serial, list(movedbpl.lev2, unit), length))
p.out.bpl.district <- prop.table(p,2)[2,]

#sample prop of those in current unit that have changed "locality" in the past 5 years
p <- with(zam, tapply(serial, list(mg5, unit), length))
p.change.local <- prop.table(p,2)[2,]

### By birthplace region 

#sample prop of those in birthplace level 1 (region) that have exited the region 
p <- with(zam, tapply(serial, list(movedbpl.lev1, bpl.lev1), length))
p.exit.bpl.region <- prop.table(p,2)[2,]

#sample prop of those in birthplace level 1 (region) that have moved within the region
p <- with(zam, tapply(serial, list(intrareg.bplmove, bpl.lev1), length))
p.move.within.bplregion <- prop.table(p,2)[2,]

###By birthplace district 

#sample prop of those in birthplace level 2 (district) that have exited the district 
p <- with(zam, tapply(serial, list(movedbpl.lev2, bpl.lev2), length))
p.exit.bpl.dist <- prop.table(p,2)[2,]

#sample prop of those in birthplace level 2 (district) that have exited the region
p <- with(zam, tapply(serial, list(movedbpl.lev1, bpl.lev2), length))
p.exit.bplregion.by.dist <- prop.table(p,2)[2,]


############# EXPORT DATA ##################

#get list of string labels for numeric factor values
lev1.lab <- attributes(zam)$label.table$geo1b_zm_lbl
lev2.lab <- attributes(zam)$label.table$geo2b_zm_lbl
bpl.lab <- attributes(zam)$label.table$bplzm_lbl
unit.lab <- attributes(zam)$label.table$geo2b_zm_lbl

#Create dataset-specific unit labels
unit.lab2 <- unit.lab[match(unit.lab, as.numeric(names(p.out.bpl.region)))]
unit.lab2 <- unit.lab2[!is.na(unit.lab2)]

bpl.lev2.lab <- bpl.lab[match(bpl.lab, as.numeric(names(p.exit.bpl.dist)))]
bpl.lev2.lab <- bpl.lev2.lab [!is.na(bpl.lev2.lab)]

bpl.lev1.lab <- lev1.lab[match(lev1.lab, as.numeric(names(p.exit.bpl.region)))]
bpl.lev1.lab <- c(bpl.lev1.lab, 99)

mobility <- as.data.frame(cbind(names(unit.lab2), unit.lab2, p.out.bpl.region, 
                  p.out.bpl.district, p.change.local))
mobility$country <- cname 
mobility$year <- year

bpl.district.mobility <- as.data.frame(cbind(names(bpl.lev2.lab), bpl.lev2.lab, p.exit.bpl.dist, p.exit.bplregion.by.dist))
bpl.district.mobility$country <- cname
bpl.district.mobility$year <- year

bpl.regional.mobility <- as.data.frame(cbind(names(bpl.lev1.lab), bpl.lev1.lab, p.exit.bpl.region,
                               p.move.within.bplregion))
bpl.regional.mobility$country <- cname
bpl.regional.mobility$year <- year

##R DATA
save(mobility, file=paste0(saveloc, "03 Mobility\\", country, year, ".mobility",  ".RData"))
save(bpl.district.mobility, file=paste0(saveloc, "05 Subreg Mobility\\", country, year, ".bpl.dist.mobility", ".RData"))
save(bpl.regional.mobility, file=paste0(saveloc, "04 Reg Mobility\\", country, year, ".bpl.reg.mobility", ".RData"))


#####################################




################ OLD CODE STARTS HERE #########################

##############################################################




#### Functions ######## 

my.ci <- function(mean, sd, n){
  m <- mean
  s <- sd
  n <- n
  error <- qnorm(0.975)*s/sqrt(n)
  lower <- m - error
  upper <- m + error 
  output <- t(c(lower, upper))
  output <- as.data.frame(output)
  colnames(output) <- c("lower", "upper")
  output
}



###########################
movers.tab.dist <- table(zam$moved, zam$geo2b.zm)[2, ]
movers.dist <- as.vector(movers.tab.dist)
names(movers.dist) <- names(movers.tab.dist)

movers.tab.const <- table(zam$moved, zam$constzm)[2, ]
movers.const <- as.vector(movers.tab.const)
names(movers.const) <- names(movers.tab.const)

movers.tab.bpl <- table(zam$moved, zam$bplzm)[2, ]
movers.bpl <- as.vector(movers.tab.bpl)
names(movers.bpl ) <- names(movers.tab.bpl)


#Export 
prop.moved.dist00 <- movers.dist / districts
prop.moved.const00 <- movers.const / const 
prop.moved.bpl00 <- movers.bpl / bpls

write.csv(prop.moved.const00, file="movers00const.csv")
write.csv(prop.moved.dist00, file="movers00dist.csv")
write.csv(prop.moved.bpl00, file = "movers00bpl.csv" )

prop.moved.dist10 <- movers.dist / districts
prop.moved.const10 <- movers.const / const 
prop.moved.bpl10 <- movers.bpl / bpls

write.csv(prop.moved.const10, file="movers10const.csv")
write.csv(prop.moved.dist10, file="movers10dist.csv")
write.csv(prop.moved.bpl10, file = "movers10bpl.csv" )





##calculate birthplace of person i score minus current place of person i score 
##For each i, get "birthplace name"'s score 

mig <-subset(fullpop, fullpop$moved==1) 

#make unique district matrix with district proportions 
stats.f <- cbind(unique(fullpop$geo2b.zm), unique(fullpop$prop.radio), 
                 unique(fullpop$prop.elctr), unique(fullpop$prop.urban), 
                 unique(fullpop$toilet.score), unique(fullpop$watsup.score))
colnames(stats.f) <- c("distid", "prop.radio", "prop.elctr", "prop.urban", "toilet.score", "watsup.score")
stats.f <- as.data.frame(stats.f)



#make container vectors for electricity and urban 
electr.p <- rep(NA, length(unique(mig$geo2b.zm)))
urban.p <- rep(NA, length(unique(mig$geo2b.zm)))
                                
for(i in 1:length(unique(mig$geo2b.zm))){
  
  sub <- subset(mig, mig$geo2b.zm==unique(fullpop$geo2b.zm)[i])
  electr.p [i] <- sub$prop.elctr[1]
  urban.p[i] <- sub$prop.urban[1] 
  
}


stats.m <- cbind(unique(mig$geo2b.zm), unique(mig$toilet.score), unique(mig$prop.tv),
                 unique(mig$watsup.score), electr.p, urban.p)


colnames(stats.m) <- c("distid",  "toilet.score", "prop.tv", "watsup.score", "prop.elctr", "prop.urban")
stats.m <- as.data.frame(stats.m)



#container vectors for individuals' birthplace scores 
bpl.prop.tv <- rep(NA, nrow(mig))
bpl.toilet.score <- rep(NA, nrow(mig))
bpl.watsup.score <- rep(NA, nrow(mig))
bpl.prop.elctr <- rep(NA, nrow(mig))
bpl.prop.urban <- rep(NA, nrow(mig))
  
#for loop to get individual's birthplace scores  

for(i in 1:nrow(mig)){
  
  #find the individual's birthplace that matches the list of unique birthplaces 
  bplid <- match(mig$bplzm[i], stats.m$distid)
  bpl.prop.tv[i] <- stats.m$prop.tv[bplid]
  bpl.toilet.score[i] <- stats.m$toilet.score[bplid] 
  bpl.watsup.score[i] <- stats.m$watsup.score[bplid]
  bpl.prop.elctr[i] <- stats.m$prop.elctr[bplid] 
  bpl.prop.urban[i] <- stats.m$prop.urban[bplid] 
  
  
}

#add to data frame 
mig$bpl.prop.tv <- bpl.prop.tv
mig$bpl.toilet.score <- bpl.toilet.score 
mig$bpl.watsup.score <- bpl.watsup.score
mig$bpl.prop.elctr <- bpl.prop.elctr
mig$bpl.prop.urban <- bpl.prop.urban

#get differences 
mig$tv.diff <- mig$prop.tv - mig$bpl.prop.tv 
mig$toilet.diff <- mig$toilet.score - mig$bpl.toilet.score
mig$water.diff <- mig$watsup.score -  mig$bpl.watsup.score
mig$elctr.diff <- mig$prop.elctr - mig$bpl.prop.elctr
mig$urban.diff <- mig$prop.urban - mig$bpl.prop.urban



#### Graph point estimates for birthplace and current place ### 

#Among those who moved up 
means <- colMeans(mig[mig$urban.diff>=0, c("bpl.prop.elctr", "prop.elctr", 
                          "bpl.prop.tv", "prop.tv",
                          "bpl.toilet.score", "toilet.score", 
                          "bpl.prop.urban", "prop.urban",
                          "bpl.watsup.score", "watsup.score")],na.rm = T)



#Plot  Means 
s <- sd(mig$bpl.prop.tv, na.rm=T)
n <- length(mig$bpl.prop.tv)
ci.mig <- my.ci(means[4], s, n)
ci.birth <- my.ci(means[3], s, n)

plot(c(1, 2), means[3:4], main = "Comparing Birth and Migration Districts", 
     ylab = "Proportions", xlab = "Districts", 
     ylim = c(0, 1), xlim = c(0, 3),
     pch = 16, type="b", xaxt = "n")
lines(c(1,1), c(ci.birth$lower, ci.birth$upper ))
lines(c(2,2), c(ci.mig$lower, ci.mig$upper ))
#urban
lines(c(1,2), means[7:8], pch=16, type="b", col="blue")
#electricity 
lines(c(1,2), means[1:2], pch=16, type="b", col="green")
#toilet 
lines(c(1,2), means[5:6], pch=16, type="b", col="red")
#water
lines(c(1,2), means[9:10], pch=16, type="b", col="purple")
axis(side = 1, at = c(1, 2), tick = FALSE,
     labels = c("Birth District", "Migration District"))


 




