##
# author @ Yunjie He
# Meal IQ Exercise
# Energy Calculation
##
install.packages('stringr')
library(stringr)
install.packages('tidytext')
library(tidytext)
install.packages('rvest')
library(rvest)
install.packages('XML')
library(XML)
install.packages('xml2')
library(xml2) 
install.packages('dplyr')
library(dplyr)
# Assumptions: 1. unit of energy is kcal
#              2. The data used for calculation is based on the top search result from two data sources.
#              3. Amount of Spaghetti depends
#              4. Language habits
#              5. The manufacturer of these food is Tesco
#              6. The weight of 3 potatoes is assumed to be 500g
#              7. The weight of 2 carrots is assumed to be 100g
#              8. The weight of 10 mushrooms is assumed to be 200g
#              9. The weight of 25 broad beans is assumed to be 125g
#             10. Romanesco represents the Romanesco Brocoli(The total weight is assumed to be 150g)
#             11. the amount of Spaghetti used is equal to the 1/3 of the total of other materials

# Step1: tidy up the data into table
#
total <- c('2 tbsp of olive oil','2 cloves of garlic','1 cup of chopped onions','2 litres of vegetable stock',
              '150 g Romanesco','125 g Broad Beans','20 g palm sugar',
              '500 g potatoes',
              '100 g carrots',
              '200 g button mushrooms')

calories <- rep(0,length(total))
energy <- data.frame('total' = total ,'calories' = calories)
energy$total <- as.character(energy$total)
energy$total <- strsplit(energy$total,' ')
energy$quantity <- rep(NA,length(total))
energy$unit <- rep(NA,length(total))
energy$material <- rep(NA,length(total))
for (i in 1:length(total)) {
    
    energy$quantity[i] <- energy$total[[i]][1]
    energy$material[i] <- paste0(energy$total[[i]][2:length(energy$total[[i]])],collapse = ' ')
    
    for (j in 1:length(energy$total[[i]])) {
        
        if (energy$total[[i]][j]=='of') {
            a = j-1
            b = j+1
            energy$quantity[i] <- energy$total[[i]][1]
            energy$unit[i] <- energy$total[[i]][a]
            energy$material[i] <- paste0(energy$total[[i]][b:length(energy$total[[i]])],collapse = ' ')
        }
        else if (energy$total[[i]][j]=='g'){
            a = j-1
            b = j+1
            energy$quantity[i] <- energy$total[[i]][1]
            energy$unit[i] <- 'g'
            energy$material[i] <- paste0(energy$total[[i]][b:length(energy$total[[i]])], collapse = ' ')
        }
    }
}
energy$quantity <- as.numeric(energy$quantity)
# Now we have a table clearly stating the content of recipes
# Then we search these materials on the website https://ndb.nal.usda.gov/ndb/search


url1 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45239582?fgcd=&manu=&format=&count=&max=25&offset=&sort=default&order=asc&qlookup=olive+oil&ds=&qt=&qp=&qa=&qn=&q=&ing='
url2 <- 'https://ndb.nal.usda.gov/ndb/foods/show/11215?man=&lfacet=&count=&max=25&qlookup=garlic&offset=&sort=default&format=Abridged&reportfmt=other&rptfrm=&ndbno=&nutrient1=&nutrient2=&nutrient3=&subset=&totCount=&measureby=&Qv=1&Q327585=1&Q327586=1&Q327587=1&Q327588=1&Qv=1&Q327585=1&Q327586=1&Q327587=1&Q327588=3.0'
url3 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45287906?man=&lfacet=&count=&max=25&qlookup=chopped+onion&offset=&sort=default&format=Full&reportfmt=other&rptfrm=&ndbno=&nutrient1=&nutrient2=&nutrient3=&subset=&totCount=&measureby=&Q511880=1&Qv=1&Q511880=0.66&Qv=1'
url4 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45316575?fgcd=&manu=&format=&count=&max=25&offset=&sort=default&order=asc&qlookup=vegetable+stock&ds=&qt=&qp=&qa=&qn=&q=&ing='
url5 <- 'https://www.tesco.com/groceries/en-GB/products/267393770'
url6 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45004375?man=&lfacet=&count=&max=25&qlookup=broad+beans&offset=&sort=default&format=Full&reportfmt=other&rptfrm=&ndbno=&nutrient1=&nutrient2=&nutrient3=&subset=&totCount=&measureby=&Q345389=1&Qv=1&Q345389=0.25&Qv=1'
url7 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45104561?fgcd=&manu=&format=&count=&max=25&offset=&sort=default&order=asc&qlookup=palm+sugar&ds=&qt=&qp=&qa=&qn=&q=&ing='
url8 <- 'https://www.tesco.com/groceries/en-GB/products/258423755'
url9 <- 'https://www.tesco.com/groceries/en-GB/products/258423755'
url10 <- 'https://ndb.nal.usda.gov/ndb/foods/show/45197437?man=&lfacet=&count=&max=25&qlookup=button+mushroom&offset=&sort=default&format=Full&reportfmt=other&rptfrm=&ndbno=&nutrient1=&nutrient2=&nutrient3=&subset=&totCount=&measureby=&Q441197=1&Qv=1&Q441197=0.5&Qv=1'
url <- c(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10)

# create a table for inforamtion from web

for (i in 1:10){
    table[[i]] <- read_html(url[i])%>%html_node("table")%>%html_table(fill=TRUE)
}

# Now, we calculate calories for each.
#1.
energy$calories[1] <- table[[1]][[4]][1]*energy$quantity[1]

#***2.
energy$unit[2] <- strsplit(names(table[[2]])[7],'')[[1]][9]
energy$quantity[2] <- as.numeric(strsplit(names(table[[2]])[7],'')[[1]][7]) * (energy$quantity[2])
url[2] <- 'https://www.tesco.com/groceries/en-GB/products/263386065' 
table[[2]] <- read_html(url[2])%>%html_node("table")%>%html_table(fill=TRUE)
cal <- strsplit(table[[2]][[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100 <- as.numeric(paste0(cal[a:b],collapse = ''))
        energy$calories[2] <- cal_100 * (energy$quantity[2]/100)
        break
    }
}

#3.
energy$calories[3] <- table[[3]][[4]][1]*energy$quantity[3]

#***4.
url[4] <- 'https://www.tesco.com/groceries/en-GB/products/259339243'
table[[4]] <- read_html(url[4])%>%html_node("table")%>%html_table(fill=TRUE)
cal <- strsplit(table[[4]][[3]][2],' ')[[1]][1] %>% as.numeric()
energy$calories[4] <- cal*energy$quantity[4]*10

#5.
cal <- strsplit(table[[5]][[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100 <- as.numeric(paste0(cal[a:b],collapse = ''))
        energy$calories[5] <- cal_100 * (energy$quantity[5]/100)
        break
    }
}

#6.
energy$calories[6] <- table[[6]][[5]][1]*(energy$quantity[6]/100)
#7.
energy$calories[7] <- table[[7]][[5]][1]*(energy$quantity[7]/100)
#8.
cal <- strsplit(table[[8]][[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100 <- as.numeric(paste0(cal[a:b],collapse = ''))
        energy$calories[8] <- cal_100 * (energy$quantity[8]/100)
        break
    }
}
#9
cal <- strsplit(table[[9]][[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100 <- as.numeric(paste0(cal[a:b],collapse = ''))
        energy$calories[9] <- cal_100 * (energy$quantity[9]/100)
        break
    }
}
#10.
energy$calories[10] <- table[[10]][[5]][1]*(energy$quantity[10]/100)



# Spaghetti to serve # Energy calculation
Spaghetti <- data.frame(total = "Spaghetti to serve")
Spaghetti$calories <- 0
Spaghetti$quantity <- 0
Spaghetti$unit <- 0
Spaghetti$material <- strsplit(as.character(Spaghetti$total),' ')[[1]][1]
 ## Let's assume that the amount of Spaghetti used is equal to 1/3 of the total of other materials
weight_other <- rep(0,10)

#1.
url1_tesco <- 'https://www.tesco.com/groceries/en-GB/products/254918424'
table_tesco <- read_html(url1_tesco)%>%html_node("table")%>%html_table(fill=TRUE)
cal <- strsplit(table_tesco[[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100g <- as.numeric(paste0(cal[a:b],collapse = ''))
        weight_other[1] <- (energy$calories[1]/cal_100g)*100
        break
    }
}

#2.
weight_other[2] <- energy$quantity[2]

#3.
weight_other[3] <- ((energy$calories[3]/table[[3]][[5]][1]) %>% as.numeric())*100

#4.
weight_other[4] <- (energy$calories[4]/(strsplit(table[[4]][[2]][2],' ')[[1]][1] %>% as.numeric()))*100
#5-10
for(i in 5:10){
    weight_other[i] <- energy$quantity[i]
}

Spaghetti$quantity <- sum(weight_other)/3
Spaghetti$unit <- 'g'

url_Spa_tesco <- 'https://www.tesco.com/groceries/en-GB/products/297844134'
table_tesco_Spa <- read_html(url_Spa_tesco)%>%html_node("table")%>%html_table(fill=TRUE)
cal <- strsplit(table_tesco_Spa[[2]][1],'')[[1]]
for (i in 1:length(cal)){
    if(cal[i]=="("){
        a <- i + 1
        b <- length(cal)-5
        cal_100g <- as.numeric(paste0(cal[a:b],collapse = ''))
        Spaghetti$calories <- cal_100g*Spaghetti$quantity/100
        break
    }
}

#combine 2 tables
Output <- rbind(energy,Spaghetti)

#refine the table
names(Output)[2] <- "calories(kcal)"
View(Output)
cat('In terms of the receipe given,the total energy is',sum(Output$`calories(kcal)`),'Kcal')











