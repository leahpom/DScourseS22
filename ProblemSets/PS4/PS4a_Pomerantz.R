# library statements
library(jsonlite)
library(tidyverse)

# 5(a) get the data, call the file "dates"
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&%20end_date=20220219&lang=en"')

# 5(b) print the file to the console 
system('cat dates.json')

# 5(c) convert to a data frame
mylist <- fromJSON('dates.json') # convert to a list
mydf <- bind_rows(mylist$result[-1]) # convert to a dataframe, removing the first row

# 5(d) check the data type
class(mydf)
class(mydf$date)

# 5(e) list the first "n" rows
head(mydf, n = 10) # let n = 10, but know that this can be any number
