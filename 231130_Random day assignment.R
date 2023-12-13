#' Assigning a random day of National Child Measurement Programme measurement to records with an existing month and year
#' Dates of measurment should not include school holidays, bank holidays or weekends
#' Script written by Professor Mario Cortina Borja and Nicola Firman
#' Last updated 11.03.2022


# Install packages
install.packages("magrittr", repos="https://cran.ma.imperial.ac.uk")
require(lubridate); require(ggplot2)

# Date formatting
format.date <- "%d/%m/%Y"

# Create bank holiday values in 2013-2019
bh <- scan(what='')
26/08/2013
25/12/2013
26/12/2013
01/01/2014
18/04/2014
21/04/2014
05/05/2014
26/05/2014
25/08/2014
25/12/2014
26/12/2014
01/01/2015
03/04/2015
06/04/2015
04/05/2015
25/05/2015
31/08/2015
25/12/2015
28/12/2015
01/01/2016
25/03/2016
28/03/2016
02/05/2016
30/05/2016
29/08/2016
26/12/2016
27/12/2016
02/01/2017
14/04/2017
17/04/2017
01/05/2017
29/05/2017
28/08/2017
25/12/2017
26/12/2017
01/01/2018
30/03/2018
02/04/2018
07/05/2018
28/05/2018
27/08/2018
25/12/2018
26/12/2018
01/01/2019
19/04/2019
22/04/2019
06/05/2019
27/05/2019
26/08/2019
25/12/2019
26/12/2019
01/01/2020

bh <- lubridate::dmy( bh, format=format.date)
bh <- bh[-length(bh)]

# Create school holiday exclusion dates for each calendar year
excl13<- c( seq( lubridate::dmy("01-01-2013"), lubridate::dmy("01-08-2013"),by='1 days'),
            seq( lubridate::dmy("28-10-2013"), lubridate::dmy("01-11-2013"),by='1 days'),
            seq( lubridate::dmy("23-12-2013"), lubridate::dmy("31-12-2013"),by='1 days') )

excl14<- c( seq( lubridate::dmy("01-01-2014"), lubridate::dmy("03-01-2014"),by='1 days'),
            seq( lubridate::dmy("17-02-2014"), lubridate::dmy("21-02-2014"),by='1 days'),
            seq( lubridate::dmy("07-04-2014"), lubridate::dmy("18-04-2014"),by='1 days'),
            seq( lubridate::dmy("26-05-2014"), lubridate::dmy("30-05-2014"),by='1 days'),
            seq( lubridate::dmy("28-07-2014"), lubridate::dmy("29-08-2014"),by='1 days'),
            seq( lubridate::dmy("27-10-2014"), lubridate::dmy("31-10-2014"),by='1 days'),
            seq( lubridate::dmy("22-12-2014"), lubridate::dmy("31-12-2014"),by='1 days') )

excl15<- c( seq( lubridate::dmy("01-01-2015"), lubridate::dmy("02-01-2015"),by='1 days'),
            seq( lubridate::dmy("16-02-2015"), lubridate::dmy("20-02-2015"),by='1 days'),
            seq( lubridate::dmy("30-03-2015"), lubridate::dmy("10-04-2015"),by='1 days'),
            seq( lubridate::dmy("25-05-2015"), lubridate::dmy("29-05-2015"),by='1 days'),
            seq( lubridate::dmy("27-07-2015"), lubridate::dmy("29-08-2015"),by='1 days'),
            seq( lubridate::dmy("26-10-2015"), lubridate::dmy("30-10-2015"),by='1 days'),
            seq( lubridate::dmy("21-12-2015"), lubridate::dmy("31-12-2015"),by='1 days') )

excl16<- c( seq( lubridate::dmy("01-01-2016"), lubridate::dmy("04-01-2016"),by='1 days'),
            seq( lubridate::dmy("15-02-2016"), lubridate::dmy("19-02-2016"),by='1 days'),
            seq( lubridate::dmy("28-03-2016"), lubridate::dmy("08-04-2016"),by='1 days'),
            seq( lubridate::dmy("30-05-2016"), lubridate::dmy("03-06-2016"),by='1 days'),
            seq( lubridate::dmy("31-07-2016"), lubridate::dmy("29-08-2016"),by='1 days'),
            seq( lubridate::dmy("24-10-2016"), lubridate::dmy("28-10-2016"),by='1 days'),
            seq( lubridate::dmy("21-12-2016"), lubridate::dmy("31-12-2016"),by='1 days'))

excl17<- c( seq( lubridate::dmy("01-01-2017"), lubridate::dmy("02-01-2017"),by='1 days'),
            seq( lubridate::dmy("13-02-2017"), lubridate::dmy("17-02-2017"),by='1 days'),
            seq( lubridate::dmy("03-04-2017"), lubridate::dmy("17-04-2017"),by='1 days'),
            seq( lubridate::dmy("29-05-2017"), lubridate::dmy("02-06-2017"),by='1 days'),
            seq( lubridate::dmy("29-07-2017"), lubridate::dmy("03-09-2017"),by='1 days'),
            seq( lubridate::dmy("23-10-2017"), lubridate::dmy("27-10-2017"),by='1 days'),
            seq( lubridate::dmy("21-12-2017"), lubridate::dmy("31-12-2017"),by='1 days'))

excl18<- c( seq( lubridate::dmy("01-01-2018"), lubridate::dmy("02-01-2018"),by='1 days'),
            seq( lubridate::dmy("12-02-2018"), lubridate::dmy("16-02-2018"),by='1 days'),
            seq( lubridate::dmy("02-04-2018"), lubridate::dmy("13-04-2018"),by='1 days'),
            seq( lubridate::dmy("28-05-2018"), lubridate::dmy("01-06-2018"),by='1 days'),
            seq( lubridate::dmy("23-07-2018"), lubridate::dmy("03-09-2018"),by='1 days'),
            seq( lubridate::dmy("22-10-2018"), lubridate::dmy("26-10-2018"),by='1 days'),
            seq( lubridate::dmy("24-12-2018"), lubridate::dmy("31-12-2018"),by='1 days'))

excl19<- c(seq( lubridate::dmy("01-01-2019"), lubridate::dmy("07-01-2019"),by='1 days'),
           seq( lubridate::dmy("18-02-2019"), lubridate::dmy("22-02-2019"),by='1 days'),
           seq( lubridate::dmy("08-04-2019"), lubridate::dmy("22-04-2019"),by='1 days'),
           seq( lubridate::dmy("27-05-2019"), lubridate::dmy("31-05-2019"),by='1 days'),
           seq( lubridate::dmy("23-07-2019"), lubridate::dmy("31-12-2019"),by='1 days'))

# Create a vector of dates from 01.01.2013 to 31.12.2019
m1<- NA 
seq.months <- c(31,28,31,30,31,30,31,31,30,31,30,31) ## leap yrs apart
seq.months2016 <- c(31,29,31,30,31,30,31,31,30,31,30,31) ## leap yrs apart

for(j in 2013:2019) for(i in 1:12) 
{
  { 
    if(j==2016 & i==2) seq.months[2]<-29 # for leap year 2016
    aux<-  paste(  1:seq.months[i], i, j,sep='/')
    m1<- c( m1, aux)
  } 
} 
m1<- m1[-1]
length(m1) 

seq.months<- c(31,28,31,30,31,30,31,31,30,31,30,31) 

# Create days of the week
Days <- lubridate::dmy( m1)
dfDays <- data.frame(Days, weekdays(Days))
names(dfDays) <- c('dates', 'dayOfWk')
dfDays <- na.omit( dfDays)

exclusion <- sort(c(bh,excl13, excl14, excl15, excl16, excl17, excl18, excl19 ) ) # vector of dates to be excluded
length(exclusion) 

test <- is.element( as.character(dfDays$dates),as.character(exclusion)) # marks bank holidays 

b1 <- !(test | (dfDays$dayOfWk=='Saturday' |  dfDays$dayOfWk=='Sunday') )
length(b1) 
table(b1) 

head( dfDays[b1, ]) 
tail( dfDays[b1, ]) 

dfDays <- dfDays[b1, ] # selects possible sampling dates
dim(dfDays) 
dfDays$month <- lubridate::month(dfDays$dates)
dfDays$year <- lubridate::year(dfDays$dates)

# Import csv file containing NCMP dates. File should contain 3 columns: person ID, month of measurement, year of measurement  
ex1 <- read.csv("XXX.csv", header=TRUE)

dim(ex1) 
ex1 <- as.data.frame(ex1); names( ex1)<- c('ID','month','year')
for(j in 1:3) ex1[,j]<- as.numeric(as.character(ex1[,j])) # moving ex1 to data.frame creates factors

format.date<- "%d/%m/%Y"

n.records<- dim(ex1)[1] # number of records in ex1
seq.months <- c(31,28,31,30,31,30,31,31,30,31,30,31) # leap yrs apart - restore
seq.months2016 <- c(31,29,31,30,31,30,31,31,30,31,30,31) # 2016 with leap year

simulated.days <- rep(bh[1], n.records) # initialise with arbitrary date
set.seed(1111) # to replicate sampling

vecDays<- paste(as.character(dfDays$month), as.character(dfDays$year), sep='-')

for(i in 1:n.records)
{
  if(i %% 1000 == 0) print(i) 
  auxM <- ex1[i,2]; auxY <- ex1[i,3] 
  auxDate <- paste(as.character(auxM), as.character(auxY), sep='-')
  b0 <- is.element( auxDate, vecDays)
  if(!b0) simulated.days[i] <- NA
  else
  { seq.aux <- seq.months
  if(auxY==2016) seq.aux<- seq.months2016
  b1 <- TRUE  
  while(b1)
  {
    day.aux <- sample(1:seq.aux[auxM],1) 
    date.aux <- lubridate::dmy(paste(day.aux, auxM, auxY, sep='/'), format=format.date)[1]
    simulated.days[i] <- date.aux
    b1 <- !is.element(date.aux, dfDays$dates)
  } 
  }
}

# Checks
table( weekdays(simulated.days))
head(cbind( ex1, simulated.days)) 
sum(is.na( simulated.days))

b0 <- is.na(simulated.days)

result <- cbind( ex1, simulated.days) # create data.frame result

result[b0, ] # look up NA cases

# Finally, export as csv
