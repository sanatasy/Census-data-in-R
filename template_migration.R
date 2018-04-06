
##R file for migration paper 
##Started 09/2015 
##Uses IPUMS census data 
##This is the template file, which uses Zambia as example 
##The main tasks this file executes are: 
##1) determine birthplace and current location mismatches 
##2) identify the type of move that occurred (across regions (level 1) or across districts (level 2)) 
##3) calculate the proportion of regional and district movers by administrative unit 
##4) export the resulting migration dataset to RData 

#### Packages ##### 
require(foreign)
library(readstata13)
####################

###### Header ######
saveloc <- "C:\\Users\\Sanata\\Dropbox\\08 Migration Paper\\Datasets\\"
seed <- "C:\\Users\\Sanata\\Dropbox\\08 Migration Paper\\IPUMS Migration\\"
location <- paste0(seed, "Zambia\\")
setwd(location)

#set country and census years 
country <- "zambia"
cname <- "Zambia"
years <- c("90","00", "10")
####################



####### Read file ############
year <- years[1]  # select a census year 
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








