# install and load the required packages
#install.packages("httr")
library(httr)
#install.packages("rvest")
library(rvest)
#install.packages("plotly")
library(plotly)

# scrape the table from webpage
pageURL <- "https://www.realclearpolitics.com/epolls/other/president_trump_job_approval-6179.html#polls"
pageContent <- httr::GET(pageURL)
allTables <- rvest::html_table(content(pageContent))
pollingData <- allTables[[4]]

# get normal pollester name as displayed in the website 
pollesterNames =unique(read_html(pageURL)%>% html_nodes('.normal_pollster_name') %>%html_text())
pollesterNames

# Remove the first row. This is RCP average row
pollingData <- pollingData[-1,]

# This is a data frame defined to hold the average approval and disapproval for
# polls for each date
cumulativePollingData = data.frame(Date = as.Date(character()),
                                   Approve = double(), Disapprove = double(),
                                   Spread = double(), 
                                   stringsAsFactors = FALSE)
# This function append year to date.
# input : list of date in the format month/day
# output: list of date in the format month/day/year
addYearToDate <- function(arg1)
{
  dateList = arg1
  lastMonth = 1
  year = 17
  l = length(dateList)
  for(i in l:1)
  {
    monthAndDate = unlist(strsplit(dateList[i],'/'))
    # when there is a transition in month from 12 to 1 increment year by one
    # Expecting minimum one poll in the month of decemer and january
    if(lastMonth == 12 && monthAndDate[1] ==1)
    {
      year = year +1
    }
    dateList[i] = paste(dateList[i], year,sep='/')
    lastMonth = monthAndDate[1]
  }
  return (dateList)
}

sepDateList <- unlist(strsplit(pollingData$Date, '-'))
startDateList <- sepDateList[c(TRUE,FALSE)]
# trim trailing space at right end
startDateList <- trimws(startDateList, which = "right")
# Append year to date"
startDateList <- addYearToDate(startDateList)
startDateList <- as.Date(startDateList, "%m/%d/%y")
endDateList <-  sepDateList[c(FALSE,TRUE)]
# trim trailing space at left end
endDateList <- trimws(endDateList, which = "left")
# Append year to date"
endDateList <- addYearToDate(endDateList)
endDateList <- as.Date(endDateList, "%m/%d/%y")

colnames(pollingData)[2] <- "Start Date"
#Adding new column end date
pollingData <- cbind(pollingData, EndDate=endDateList)
# Change order of columns so that start and end dates are nex to each other
pollingData <- pollingData[,c(1,2,7,3,4,5,6)]
pollingData$`Start Date`= startDateList


totalPollForDate <- function (date)
{
  givenDate = date
  temp1= subset(pollingData, (pollingData$EndDate <= givenDate))
  
  totalRows = nrow(temp1)
  temp2 = temp1[1,]
  for (i in 2:totalRows)
  {
    if (!(temp1$Poll[i] %in% temp2$Poll))
    {
      if(nrow(temp2) < 11)
      {
        temp2 = rbind(temp2,temp1[i,] )
      }
    }
    
  }
  AvgApprove = round(mean(temp2$Approve),1) 
  AvgDisapprove = round(mean(temp2$Disapprove),1)
  calSpread = AvgApprove - AvgDisapprove
  return(data.frame(Date = givenDate, Approve=AvgApprove,
                    Disapprove=AvgDisapprove, 
                    Spread=calSpread, stringsAsFactors=FALSE)) 
  
}

createCumTableForAllDates <- function()
{
  pollDate = as.Date("1/27/17", "%m/%d/%y") # Start date is 27/Jan/2017 till todays date
  
  while(pollDate< Sys.Date())
  {
    cumulativePollingData = rbind(cumulativePollingData, totalPollForDate(pollDate))
    pollDate = pollDate +1
  }
  return(cumulativePollingData)
}


cumulativePollingData = createCumTableForAllDates()
globalAvgApprove = round(mean(cumulativePollingData$Approve),1)
globalAvgDisApprove = round(mean(cumulativePollingData$Disapprove),1)

t <- list(
  family = "sans serif",
  size = 14)



 trumpJobGraph= plot_ly(x = cumulativePollingData$Date)%>%
  add_trace(y = cumulativePollingData$Approve,
            mode = 'lines', color = I('black'), name = paste(globalAvgApprove,"Approve"), 
             type = 'scatter',hoverinfo = 'x+y') %>%
  add_trace(y = cumulativePollingData$Disapprove, 
            mode = 'lines', color = I('red'), name = paste(globalAvgDisApprove,"Disapprove"), 
             type = 'scatter',hoverinfo = 'x+y') %>%
  add_trace(y= cumulativePollingData$Spread, mode = "lines", color = I('red'),
            fill = 'tozeroy', fillcolor = 'red', hoverinfo = 'x+y', type = 'scatter',
            name = "Spread" )%>%
  layout(title = 'President Trump Job Approval', font = t, yaxis = list(dtick = 5),
         xaxis = list( 
           rangeselector = list( method = "relayout",
             buttons = list(
               list( 
                    count = 7,
                    label = "7D",
                    step = "day",
                    stepmode = "backward"),
               list(
                 count = 14,
                 label = "14D",
                 step = "day",
                 stepmode = "backward"
               ),
               list(
                 count = 30,
                 label = "30D",
                 step = "day",
                 stepmode = "backward"
               ),
               list(
                 count = 6,
                 label = "6M",
                 step = "month",
                 stepmode = "backward"
               ),
               list(
                 count = 1,
                 label = "1Y",
                 step = "year",
                 stepmode = "backward"
               ),
               list(
                 label = "All",
                 step = "all")))
           )
         )
saveRDS(trumpJobGraph,"TrumpJobApprovalGraph.rds")

