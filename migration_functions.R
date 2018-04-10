##R script of functions used for migration dataset 
##Started 09/2016 
##These funcitons read international IPUMS census data to identify and disaggregate geographic variables at various administrative 
##unit levels, using Cameroon's 2005 census as an example
##The main tasks this file executes are: 
##1) determine birthplace and current location mismatches 
##2) identify the type of move that occurred (across regions (level 1) or across districts (level 2)) 
##3) export the resulting migration dataset to RData 


#### Packages ##### 
require(foreign)
library(plyr)
####################


##### FUNCTIONS ##### 

sortlevels <- function(df, levelnames){ 
  #this function sorts a given character vector of administrative unit names (levelnames) from 
  #lowest to highest level 
  
  getmax <- sort( sapply( 1:length(levelnames), function(x) max((df[, levelnames[x]])) ) )
  levelnames <- levelnames[order(getmax)]
  return(levelnames)
  
}

getlevelsAll <- function(df){
  #this function identifies the current administrative units at all available levels 
  #for IPUMS census respondents, from lowest to highest level 
  #output: a matrix of all geographic variables with columns labeled lev1, lev2, lev3, etc.

  #get location of level 1 and 2 adm units 
  lev1.loc <- grep("geo1b", colnames(df), value=F)[1]
  lev2.loc <- grep("geo2b", colnames(df), value=F)[1]
  
  #assign new level 1 and 2 variables  
  lev1 <-    df[, lev1.loc ]
  lev2 <-    df[, lev2.loc ]
  
  ##find any remaining administrative units 
  admgap <- grep("ownrshp", colnames(df), value=F)[1] -         #ownrshp is the first question asked after geography
           lev2.loc - 1
  morelevels <- ifelse(admgap == 0, 0, 1)                       #check if more lower-level units 
  
  #return 3 or more levels
  if(morelevels==1){
     levelvars <- colnames(df)[ (lev2.loc + 1) : (lev2.loc + admgap)]
     addlevels <- as.data.frame(df[, levelvars])
     colnames(addlevels) <- sapply(1:ncol(addlevels), function(x) paste0("lev", x+2))
     lev.mtx <- cbind(lev1, lev2, addlevels)
     names(lev.mtx) <- c("lev1", "lev2", colnames(addlevels))
     return( lev.mtx )
   }
  
  #return 2 levels 
  lev.mtx <- cbind(lev1, lev2)
  names(lev.mtx) <- c("lev1", "lev2")
  return( lev.mtx )
  
}


getlevel <- function(df, level, levelnames){
  #This function identifies the administrative level of a given administrative unit string (level)
  #from a character vector of possible administrative units (levelnames), ordered from lowest to highest level
  #It is useful for variables that are not labeled with a adminn unit level number such as birthplace unit or any unit greater than "geo2b" 
  #It returns a list with the following elements: the name of the appropriate admin unit, the number (integer) of the unit,
  #and a renamed column vector with the appropriate unit 
  
  #remove non-response values 
  df[, level] <- ifelse(df[, level] >=9997, NA, df[, level]) 
  
  #Determine the administrative unit of the birthplace variable: 
  #Check if the level variable is beyond the max value of each admin unit variable in the vector of levelnames
  #A false value means there are more bpl units than the admin unit in question, indicating no match 
  #The first true value means all bpl units are contained within the admin unit in question, indicating a match
  levMatch <- sapply(1:length(levelnames), function(x) any(df[, level] > max(df[, levelnames[x]]), na.rm=T))
  
  if(all(levMatch==F)){
    return(NULL)
  }
  
  else{
  #return match 
  level.df <- as.data.frame(df[, level])
  colnames(level.df) <- levelnames[which(levMatch==F)]
  
  #produce a list of the matching admin unit name, and its corresponding column vector in df 
  results <- list(levelnames[which(levMatch==F)], which(levMatch==F), level.df )
  
  return(results)
  }
  
}


getlevelbpl <- function(df, levelnames){
  #this function operates the same as the getlevel function, but it identifies the corresponding 
  #administrative unit specifically for the IPUMS "birthplace" variable from a character vector 
  #of possible administrative units (levelnames), ordered from lowest to highest level
  #It returns a list with the following elements: the name of the appropriate admin unit, the number (integer) of the unit,
  #and a renamed column vector with the appropriate unit  
  
  #get 2-letter country code ("geo1b" is a common variable name across all IPUMS datasets, appended by the 2-letter country code)
  lev1.loc <- grep("geo1b", colnames(df), value=F)[1]
  lev1name <- colnames(df)[lev1.loc]
  ctry <- substr(lev1name, nchar(lev1name)-1, nchar(lev1name) )           
  
  #assign birthplace variable ("bpl" is a common variable name across all IPUMS datasets, appended by the 2-letter country code) 
  bpl.loc <- grep(paste0("bpl", ctry), colnames(df), value=F)
  bpl <- df[, bpl.loc]
  bpl <- ifelse(bpl >=9997, NA, bpl) #remove non-response values 
  
  #determine the administrative unit of the birthplace variable: 
  #check if the bpl variable is beyond the max value of each admin unit variable 
  #a false value means there are more bpl units than the admin unit in question
  #the first true value means all bpl units are contained within the admin unit in question, indicating a match
  bplmatch <- sapply(1:length(levelnames), function(x) any(bpl > max(df[, levelnames[x]]), na.rm=T))
  
  if(all(bplmatch==F)){
    return(NULL)
  }
  
  #return match 
  bpl <- as.data.frame(bpl)
  colnames(bpl) <- paste0("bpl.", levelnames[which(bplmatch==F)])
  
  #produce a list of the matching admin unit name, and its corresponding column vector in df 
  results <- list(levelnames[which(bplmatch==F)], which(bplmatch==F), bpl )
  
  return(results)

}
 

disagg <- function(df, level, levelnames){
  #this function returns the corresponding lower-level administrative units for a given 
  #unit (level), choosing from a list of potential administrative units (levelnames) 
  #ordered from lowest to highest level 
  #it identifies "level's" position in the list of levelnames, then returns 
  #the corresponding administrative name of all units lower than "level", in ascending order 
  
  foundLev <- getlevel(df, level, levelnames)[[1]] 
  levpos <- which(foundLev %in% levelnames)[1]
  
  return(levelnames[1:(levpos-1)])
  
}


toLevel <- function(df, fromLevel, toLevel, levelnames){
  #input: an unidentified administrative unit (fromLevel), the desired level of disaggregation (toLevel) 
  #toLevel only accepts "lev1" or "lev2"
  #output: a named column vector of the corresponding toLevel unit 
  #NOTE1: this function uses my custom getlevel() function, above  
  #NOTE2: this function must start with the level 2 unit because not all level2 codes
  #have level1 codes as a node (level2 does not always equal level1 string + level2 string)
  #NOTE3: however, this function assumes that administrative levels higher than 2 link back to 
  #level 2 (i.e. are concatenated), per IPUMS documentation. This assumption is needed to "strip" 
  #higher levels down to level 2 
  
  #identify the input unit
  fromLevel.unit <- getlevel(df, fromLevel, levelnames)[[1]]
  
  if(fromLevel.unit=="lev1"){ #if the "from" admin unit is already the lowest unit, return null
     return(NULL)
  }
  
  #identify level 1 and 2 variables 
  lev1.loc <- grep("geo1b", colnames(df), value=F)[1]
  lev2.loc <- grep("geo2b", colnames(df), value=F)[1]
  
 
  ##extract the level 2 unit: 
  
    #get range of nchar for input unit and level 2 units  
    fromLevel.range <- range(nchar(df[, fromLevel]))
    lev2.range <- range(nchar(df[, lev2.loc]))
  
    #substring the level 2 unit from the "fromLevel" variable, allowing for varying digits 
    sub <- ifelse(nchar(df[, fromLevel])==fromLevel.range[2], 
                          substr(df[, fromLevel], 1, lev2.range[2]), 
                          substr(df[, fromLevel], 1, lev2.range[1]))
    sub <- as.numeric(sub)
    #match the extracted substring to Level 2 unit to get an index 
    findLev2 <- df[match(sub, df[, lev2.loc]), lev2.loc] 
   
    
    if(toLevel=="lev2"){
      return( setNames(as.data.frame(findLev2), paste0(fromLevel, ".", toLevel)) )
    }
    
    if(toLevel=="lev1"){
      findLev1 <- df[match(sub, df[, lev2.loc]), lev1.loc ] 
      return( setNames(as.data.frame(findLev1), paste0(fromLevel, ".", toLevel)) )
    }
    
  
     return(NULL)
  
 
}


### APPLY TO SAMPLE DATA ####

## Header
path <- "Census-data-in-R/cameroon_census05_sample100.RData"
load( path )


#test functions 
head(getlevelsAll(cam))
getlevelbpl( cam, names(getlevelsAll(cam)) )[[1]] 
head( getlevelbpl( cam, names(getlevelsAll(cam)) )[[3]] )
getlevel( cam, "bplcm", names(getlevelsAll(cam)) )[[1]]
disagg(cam, "bplcm", names(getlevelsAll(cam)))
head( toLevel(cam, 
              fromLevel="bplcm", 
              toLevel="lev1", 
              levelnames = names(getlevelsAll(cam))) )

#assign geographic units 
cam <- cbind(cam, getlevelsAll(cam))

#assign birthplace units 
cam <- cbind(cam, toLevel( cam, "bplcm", "lev1", names(getlevelsAll(cam)) ) )
cam <- cbind(cam, toLevel( cam, "bplcm", "lev2", names(getlevelsAll(cam)) ) )

##add migration indicators

#Moved from birthplace level 1 (region) 
cam$movedbpl.lev1 <- ifelse(cam$bplcm.lev1 != cam$lev1, 1, 0)
#Moved from birthplace level 2 (district)
cam$movedbpl.lev2 <- ifelse(cam$bplcm.lev2 != cam$lev2, 1, 0)
#Intra-regional move from birthplace unit 
cam$withinreg.bplmove <- ifelse(cam$bplcm.lev2 != cam$lev2 & cam$bplcm.lev1 == cam$lev1, 1, 0)
#Inter-regional move from birthplace unit 
cam$acrossreg.bplmove <- ifelse(cam$bplcm.lev2 != cam$lev2 & cam$bplcm.lev1 != cam$lev1, 1, 0)

table(cam$movedbpl.lev1)
table(cam$movedbpl.lev2)
table(cam$withinreg.bplmove)
table(cam$acrossreg.bplmove)

#save(cam, file= paste0(path, "cameroon_census05_sample100_edit.RData"))


