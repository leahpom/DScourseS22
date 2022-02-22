# library commands
library(sparklyr)
library(tidyverse)

# 6.4 install spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# 6.5 create a tibble called df1
df1 <- as_tibble(iris)

# 6.6 copy the tibble into spark 
df <- copy_to(sc, df1)

# 6.7 verify the different classes - put the answer in the LaTex document
class(df1)
class(df)

#6.8 run the column names of each
colnames(df1)
colnames(df)

# 6.9 listing the first six rows
df %>% select(Sepal_Length,Species) %>% head %>% print

# 6.10 filter command
df %>% filter(Sepal_Length>5.5) %>% head %>% print

# 6.11 put the two commands together using piplines
df %>% select(Sepal_Length,Species) %>% filter(Sepal_Length>5.5) %>% head %>% print

# 6.12 group_by command
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

# 6.13.a re-execute the call, assigning df2 to the output
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

# 6.13.b use the arrange function
df2 %>% arrange(Species) %>% head %>% print

