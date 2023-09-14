setwd("C:/Users/14166/Documents/COVID-19/Wave3")

library(ggmap)
d.. <- read.csv(file="2023_03_23_All_Waves_identifiable.csv")


d..$PCODE1 <- d..$USUHOME
d..$PCODE1[d..$USUHOME==""] <- d..$COVIDHOME[d..$USUHOME==""]

d..$X <- NA
d..$Y <- NA
d..$GoogleReturn <- NA

d..$ALTAD1 <- paste(d..$PCODE1, ", Canada", sep="")

register_google(key = "AIzaSyBNpk8VBaRGSsjlmDoBEgDK-NY6RtsqbKM")


for(i in 1:540){
    
    result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
    
    result <- data.frame(result)
    
    d..$Y[i] <- as.numeric(result$lat[1])
    d..$X[i] <- as.numeric(result$lon[1])
    d..$GoogleReturn[i] <- as.character(result$address)
    rm(result)
    print(i)
   
  
  
}

for(i in 541:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}


for(i in 709:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}


for(i in 1428:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}



for(i in 1793:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}

for(i in 1892:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}


for(i in 2294:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}


for(i in 2343:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}



for(i in 2977:nrow(d..)){
  
  result <- geocode(d..$ALTAD1[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Y[i] <- as.numeric(result$lat[1])
  d..$X[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn[i] <- as.character(result$address)
  rm(result)
  print(i)
  
  
  
}


d..$Xw2 <- NA
d..$Yw2 <- NA

d..$ALTAD2 <- paste(as.character(d..$NEWPOST), "Canada", sep=", ")
d..$GoogleReturn2 <- NA

register_google(key = "AIzaSyBNpk8VBaRGSsjlmDoBEgDK-NY6RtsqbKM")


for(i in 1:nrow(d..)){
if(!is.na(d..$NEWPOST[i]) & !d..$NEWPOST[i]==""){
  
  result <- geocode(d..$ALTAD2[i], output = "latlona", source = "google")
  
  result <- data.frame(result)
  
  d..$Yw2[i] <- as.numeric(result$lat[1])
  d..$Xw2[i] <- as.numeric(result$lon[1])
  d..$GoogleReturn2[i] <- as.character(result$address)
  rm(result)
  print(i)
}else{
  
}
  
  
  
}



d..$Xw3 <- NA
d..$Yw3 <- NA

d..$ALTAD3 <- paste(as.character(d..$PCODE22), "Canada", sep=", ")
d..$GoogleReturn3 <- NA

register_google(key = "AIzaSyBNpk8VBaRGSsjlmDoBEgDK-NY6RtsqbKM")


for(i in 1:nrow(d..)){
  if(!is.na(d..$PCODE22[i]) & !d..$PCODE22[i]==""){
    
    result <- geocode(d..$ALTAD3[i], output = "latlona", source = "google")
    
    result <- data.frame(result)
    
    d..$Yw3[i] <- as.numeric(result$lat[1])
    d..$Xw3[i] <- as.numeric(result$lon[1])
    d..$GoogleReturn3[i] <- as.character(result$address)
    rm(result)
    print(i)
  }else{
    
  }
  
  
  
}

d..$FX <- d..$X
d..$FY <- d..$Y

d..$FX[!is.na(d..$Xw2)] <- d..$Xw2[!is.na(d..$Xw2)]
d..$FY[!is.na(d..$Yw2)] <- d..$Yw2[!is.na(d..$Yw2)]

d..$FX[!is.na(d..$Xw3)] <- d..$Xw3[!is.na(d..$Xw3)]
d..$FY[!is.na(d..$Yw3)] <- d..$Yw3[!is.na(d..$Yw3)]

d..$Yw3[!is.finite(d..$Yw3)] <- NA
d..$Xw3[!is.finite(d..$Xw3)] <- NA
d..$Yw2[!is.finite(d..$Yw2)] <- NA
d..$Xw2[!is.finite(d..$Xw2)] <- NA






write.csv(d.., file="2023_03_24_All_Waves_XYs.csv")
write.csv(d..[, c("Response.ID","FX","FY")], file="Wave3_ForGeocoding.csv")


library(foreign)

s.. <- read.dbf(file="Wave3_in_DA16.dbf")

str(s..)

s..$Response.ID <- as.character(s..$Response.I)
s..$DAUID <- as.character(s..$DAUID)

d.. <- merge(d.., s..[,c("Response.ID","DAUID")], by.x="Response.ID", by.y="Response.ID", all.x=TRUE, all.y=FALSE)
d..$DA16 <- d..$DAUID.y
d..$DAUID.y <- NULL



w.. <- read.dbf(file="Wave3_in_DA21.dbf")

str(w..)

w..$Response.ID <- as.character(w..$Response.I)
w..$DA21 <- as.character(w..$DAUID)
 
d.. <- merge(d.., w..[,c("Response.ID","DA21")], by.x="Response.ID", by.y="Response.ID", all.x=TRUE, all.y=FALSE)
#d..$DA21 <- d..$DAUID.y
#d..$DAUID.y <- NULL

table(is.na(d..$DA21))

write.csv(d.., file="2023_06_15_All_Waves_XYsDAs.csv")

dx.. <- d..

dx..$FY <- NULL
dx..$FX <- NULL

dx..$S.EMAIL <- NULL
dx..$T.EMAIL <- NULL

dx..$Recipient.Email <- NULL
dx..$RecipientEmail2 <- NULL

dx..$USUHOME <- NULL
dx..$COVIDHOME <- NULL

dx..$PCODE <- NULL
dx..$PCODE1 <- NULL
dx..$PCODE22 <- NULL

dx..$PC_X <- NULL
dx..$PC_X2 <- NULL
dx..$PC_Y <- NULL
dx..$PC_Y2 <- NULL

write.csv(dx.., file="2023_06_15_All_Waves_Deidentified_DAs.csv")


c.. <- read.csv(file="C:/Users/14166/Documents/MJ All/Spatial Accessibility Measures April 6 2023/data/acs_caf.csv")

c..$DA <- as.character(substr(c..$dbuid, 1, 8))

table(c..$csdname[c..$DA %in% dx..$DA21])



#### SCRATCH


t.. <- read.csv(file="C:/Users/14166/Documents/COVID-19/TorontoJune5ANON.csv")
t.. <- read.csv(file="C:/Users/14166/Documents/COVID-19/TorontoFinalGEOCODES.csv")

table(duplicated(d..$ROWID))

d..$ROWID[d..$USUHOME=="M6P2Z7"]


t..$ROWID[t..$COVIDHOME=="V5t 1k8"]

v.. <- read.csv(file="C:/Users/14166/Documents/COVID-19/VanWorkingJune1GEOCoded.csv")

v..$ALTID <- paste(v..$IP.Address, v..$End.Date, v..$ROWID) 


d..$ALTID <- paste(d..$IP.Address, d..$End.Date, d..$ROWID) 


d..$Xv <- tapply(v..$X, v..$Response.ID, mean)[paste(d..$Response.ID)]
d..$Yv <- tapply(v..$Y, v..$Response.ID, mean)[paste(d..$Response.ID)]


library(foreign)
p.. <- read.dbf(file="C:/Users/14166/Documents/ArcGIS/TorontoInPD.dbf")

t.. <- read.csv(file="C:/Users/14166/Documents/COVID-19/TorontoJune5ANON.csv")

t..$X <- tapply(p..$X, p..$ROWID_, median)[paste(t..$ROWID)]
t..$Y <- tapply(p..$Y, p..$ROWID_, median)[paste(t..$ROWID)]


d..$Xt <- tapply(t..$X, t..$Response.ID, mean)[paste(d..$Response.ID)]
d..$Yt <- tapply(t..$Y, t..$Response.ID, mean)[paste(d..$Response.ID)]

d..$X <- d..$Xt
d..$X[is.na(d..$Xt)] <- d..$Xv[is.na(d..$Xt)]

d..$Y <- d..$Yt
d..$Y[is.na(d..$Yt)] <- d..$Yv[is.na(d..$Yt)]

table(is.na(d..$NEWPOST))