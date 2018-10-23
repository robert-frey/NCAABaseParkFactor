library(janitor)
library(rvest)
library(dplyr)
library(stringr)

#insert url
{
url <- "url"

#R reads the url as a HTML doc
my_html <- read_html(url)


# We'll access the first table from the web page(Schedule)
# Trial and Error process

my_tables <- html_nodes(my_html,"table",)[[2]]
team_table <- html_table((my_tables), fill = TRUE)
View(team_table)
team_table <- team_table[-1:-2,]
}

{
a_table <- team_table

#remove missing values if missing
#b_table = a_table[-26,]
#a_table <- b_table

#add columns

a_table$RA <-NA
a_table$RSH <-NA
a_table$RAH <-NA
a_table$RSRN <-NA
a_table$RARN <-NA
a_table$Result <-NA
}

#Name Columns
names(a_table) = c("Date","Opponent","Result","RS","RA","RSH","RAH","RSRN","RARN")

#split text from each designated column
{
str_split("Result","-")

split <- str_split(a_table$Result,"-")

a_table$RS <- sapply(split, "[[",1)
a_table$RA <- sapply(split, "[[",2)

str_split("RS", " ")
split2<- str_split(a_table$RS, " ")

a_table$Result <- sapply(split2, "[[",1)
a_table$RS <- sapply(split2, "[[",2)

str_split("RA", " ")
split3<- str_split(a_table$RA, " ")
a_table$RA <- sapply(split3, "[[",2)
}

#ifelse statement to determine whether runs were scored & allowed at home or on the road
{
a_table$RSH <- ifelse(!grepl("@", a_table$Opponent), a_table$RS, 0)
a_table$RAH <- ifelse(!grepl("@", a_table$Opponent), a_table$RA, 0)
a_table$RSRN <- ifelse(grepl("@", a_table$Opponent), a_table$RS, 0)
a_table$RARN <- ifelse(grepl("@", a_table$Opponent), a_table$RA, 0)
}

#add home and road games column
{
a_table$HG<-NA
a_table$RG<-NA

#count the amount of home and road games
a_table$RG <- str_count(a_table$Opponent, "@")
a_table$HG <- ifelse((a_table$RG == 1), 0, 1)
}

{
ws <- a_table

#convert each string to numeric
ws$RS = as.numeric(as.character(ws$RS))
ws$RA = as.numeric(as.character(ws$RA))
ws$RSH = as.numeric(as.character(ws$RSH))
ws$RAH = as.numeric(as.character(ws$RAH))
ws$RSRN = as.numeric(as.character(ws$RSRN))
ws$RARN = as.numeric(as.character(ws$RARN))
ws$HG = as.numeric(as.character(ws$HG))
ws$RG = as.numeric(as.character(ws$RG))
}

#generate totals row
final <- ws %>%
  adorn_totals("row")

#extract totals row from dataset
{
j2 <- tail(final,1)

#add iPF column and format decimal places
j2$iPF <- NA
j2$RAA <- NA
j2$iPF <- ((j2$RSH+j2$RAH)/(j2$HG))/((j2$RSRN+j2$RARN)/(j2$RG))
j2$iPF <- round(j2$iPF,3)
j2$RAA <- ifelse(j2$iPF > 1, j2$iPF-(abs(j2$iPF-1)/2), j2$iPF+(abs(j2$iPF-1)/2))
j2$RAA <- round(j2$RAA,3)
View(j2)
}

