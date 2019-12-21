#Homework2: Handling Data
library(shiny)
library(plotly)
library(ggplot2)
library(quantmod)

#Q1: Get All Firms Data
nasdaq_names = stockSymbols(exchange = "NASDAQ")
nyse_names = stockSymbols(exchange = "NYSE")
amex_names = stockSymbols(exchange = "AMEX")
df = rbind(nasdaq_names,nyse_names,amex_names)
print(dim(df))

#Q2: Clean Data
df2 = na.omit(df, cols="x")
print(dim(df2))

# or 
idx = complete.cases(df)
df2_1 = df[idx,]
print(nrow(df2_1))

#Q3: IPO Activity
library(dplyr)
library(magrittr)
df2$count = 1
df3 = dplyr::filter(df2, !is.na(IPOyear)) # Check IPOyear na values 
res = df3 %>% group_by(IPOyear) %>% summarise(numIPO = sum(count))
barplot(res$numIPO,names.arg = res$IPOyear) # Bar plot
library(rbokeh) 
p = figure(width=500,height=300) %>% 
  ly_points(IPOyear,numIPO,data=res,hover=c(IPOyear,numIPO)) %>% 
  ly_lines(IPOyear,numIPO,data=res)
p # Line plot

# or 
idx = which(!is.na(df2$IPOyear))
df3_1 = df2[idx,]
res_1 = df3_1 %>% group_by(IPOyear) %>% summarise(numIPO = sum(count))
print(res_1)

barplot(res_1$numIPO,names.arg = res$IPOyear)

library(rbokeh)
p = figure(width=500,height=300) %>% ly_points(IPOyear,numIPO,data=res,hover=c(IPOyear,numIPO)) %>% ly_lines(IPOyear,numIPO,data=res)
p

#Q4: Industry Sectors
res2 = df3 %>% group_by(Sector) %>% summarise(numCompany = sum(count)) # Frequency of firms by sector
print(res2)
print(dim(res2))
#A4: 12 sectors

# or 
idx2 = which(!is.na(df$Sector))
df2 = df[idx2,]
df2$count = 1
res2_1= df2 %>% group_by(Sector) %>% summarise(numCompany = sum(count))
print(res2)


#Q5: Finance Sector
df$MarketCap = gsub("\\$","", df$MarketCap) 
#Change Million/Billion to nature numbers
df$MarketCap = gsub('B', 'e9', df$MarketCap)
df$MarketCap = gsub('M', 'e6', df$MarketCap)
df$MarketCap = format(as.numeric(df$MarketCap), scientific = FALSE, big.mark = ",")
Finance = df %>% filter(Sector=="Finance")
# Sort by market capitalization
sort = Finance[order(Finance$MarketCap, decreasing = TRUE, na.last = TRUE ), ]
# Print top 50 
sort[1:50, c("Name", "MarketCap")] 

# or 
idx = grep("B",df$MarketCap)
x = df$MarketCap; df$MarketCap = as.numeric(substr(x,2,nchar(x)-1))
df$MarketCap[idx] = df$MarketCap[idx]*1000 #For the billion cases
idx = which(df$MarketCap>0)
df3 = df[idx,]
Finance = df3 %>% filter(Sector=="Finance")

Finance_sort = arrange(Finance,desc(MarketCap))
head(Finance_sort,50)

#Q6 Using the Data Table package
install.packages("DT")
library(DT)
datatable(Finance)
# Screenshot was uploaded with .R file to Camino

#Q7 Show the same using Shiny for finance firms 
#for your convenience, this part was created with a new file(app_hw2.R) and uploaded to camino

#GetData.R
#Subset Finance sector
nasdaq_names = stockSymbols(exchange = "NASDAQ")
nyse_names = stockSymbols(exchange = "NYSE")
amex_names = stockSymbols(exchange = "AMEX")
df = rbind(nasdaq_names,nyse_names,amex_names)
#Convert market capital values to nature numbers
#Delete "$"
df$MarketCap = gsub("\\$","", df$MarketCap) 
#Change Million/Billion to nature numbers
df$MarketCap = gsub('B', 'e9', df$MarketCap)
df$MarketCap = gsub('M', 'e6', df$MarketCap)
df$MarketCap = format(as.numeric(df$MarketCap), scientific = FALSE, big.mark = ",")
Finance = df %>% filter(Sector=="Finance")

#server.R
library(shiny)
library(ggplot2)
library(quantmod)
library(DT)
library(dplyr)
library(magrittr)

server = function(input, output, session) {
  nasdaq_names = stockSymbols(exchange = "NASDAQ")
  nyse_names = stockSymbols(exchange = "NYSE")
  amex_names = stockSymbols(exchange = "AMEX")
  df = rbind(nasdaq_names,nyse_names,amex_names)
  
  #Convert market capital values to nature numbers
  #Delete "$"
  df$MarketCap = gsub("\\$","", df$MarketCap) 
  #Change Million/Billion to nature numbers
  df$MarketCap = gsub('B', 'e9', df$MarketCap)
  df$MarketCap = gsub('M', 'e6', df$MarketCap)
  df$MarketCap = format(as.numeric(df$MarketCap), scientific = FALSE, big.mark = ",")
  Finance = df %>% filter(Sector=="Finance")
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(Finance[, input$show_vars, drop = FALSE])
  })
  
}

#ui.R
library(shiny)
library(ggplot2)
library(quantmod)
library(DT)
library(dplyr)
library(magrittr)

ui <- shinyUI(fluidPage(
  title = 'Financial Firms Data',
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Finance"',
        checkboxGroupInput('show_vars', 'Choose data elements:',
                           names(Finance), selected = names(Finance))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Finance', DT::dataTableOutput('mytable1'))
      )
    )
  )
))

shinyApp(ui = ui, server = server)