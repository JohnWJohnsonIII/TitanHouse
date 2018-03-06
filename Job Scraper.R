###################################################
#####CHECK FILE PATHS BEFORE PROCEEDING######


####comparing indeed listings for positions with
####Title containing "Vice President" "President" or "Chief"
####posted within the last 15 days (50 results per page)
####with TH-Tracked companies and PE Firms


#install.packages("rvest")
#install.packages("stringr")
#install.packages("purrr")
#install.packages("RSelenium")
#install.packages("xml2")
#install.packages("tm")
#install.packages("twitteR")
#install.packages("RCurl")
#install.packages("devtools")
#install.packages("httr")

library(rvest)
library(stringr)
library(purrr)
library(xml2)
library(plyr)
library(dplyr)
library(tm)
library(twitteR)
library(RCurl)
library(devtools)
library(httr)

#job search URL
wurl <- "https://www.indeed.com/jobs?as_and=&as_phr=&as_any=&as_not=&as_ttl=President+OR+%22Chief+Executive+Officer%22+OR+%22Chief+Financial+Officer%22+OR+%22Chief+Operating+Officer%22+OR+%22Chief+Human+Resource+Officer%22+OR+%22Chief+Technology+Officer%22+OR+%22General+Counsel%22&as_cmp=&jt=fulltime&st=&salary=&radius=25&l=&fromage=15&limit=50&sort=date&psf=advsrch"
wurl_html <- read_html(wurl)

#grabs all results links available from first page of results (first 1K jobs)
page.links <- wurl_html %>%
  html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
  html_attr('href')
page.links <- as.data.frame(page.links)

#base url for creating full URLs from page.links results
baseURL <- "http://www.indeed.com"


#create dataframe to hold results of url pasting
allURLS <- data.frame(FullURL=character(),
                      stringsAsFactors = FALSE)
colnames(allURLS) <- c("FullURL")
allURLS[1,1] <- wurl #add first page of results to allURLS df

#loop appends baseURL to page.links results and adds results to allURLS
g=2 #set counter for rows in page.links
for (i in 2:nrow(page.links)) { #starts at 2 because one of the URLs is captured twice in page.links
  next_URL <- paste(baseURL, page.links[i,], sep="")
  allURLS[g,] <- next_URL
  g = g+1
}

#View(allURLS)  #check your results from URL pasting loop


#create df to catch results of upcoming loop
jobs <- data.frame(title = character(),
                   company = character(),
                   location = character(),
                   stringsAsFactors = FALSE)

###loop scrapes through each URL 
###and writes results to vectors
for (i in 1:nrow(allURLS)) {
  
  title <- read_html(allURLS[i,]) %>% 
    html_nodes(".jobtitle") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  title <- c(title)

  
  company <- read_html(allURLS[i,]) %>% 
    html_nodes(".company") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  ompany <- c(company)

  
  location <- read_html(allURLS[i,]) %>% 
    html_nodes(".location") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  location <- c(location)

  
  scrape1 <- cbind(title, company, location)
  
  jobs <-rbind(jobs, scrape1)
  
  title = NULL
  company = NULL
  location = NULL
  
  Sys.sleep(3) #pause 3 secs before next iteration
}


View(jobs) #view scraper loop results


###compare companies in jobs df to TH-tracked companies list

#read tracked companies list
trackedco <- read.csv("C:/Users/John Johnson/Documents/TitanHouse/TH Data/TH tracked co's .csv", header = TRUE)
#View(head(trackedco))

#create data frame to hold comparison results
THJobs <- data.frame(Title=character(),
                     Company=character(),
                     Location=character(),
                     stringsAsFactors = FALSE)
colnames(THJobs) <- c("Title", "Company", "Location")


#clean datasets that are being compared
#removing common items from company names that may or may not be included in
#one dataset or another
gsub("LLC", "", jobs)
gsub(", LLC", "", jobs)
gsub("Inc", "", jobs)
gsub("Inc.", "", jobs)
gsub(", Inc", "", jobs)
gsub(", Inc.", "", jobs)

gsub("LLC", "", trackedco)
gsub(", LLC", "", trackedco)
gsub("Inc", "", trackedco)
gsub("Inc.", "", trackedco)
gsub(", Inc", "", trackedco)
gsub(", Inc.", "", trackedco)


#compare indeed companies to tracked companies
k=1 #set counter for rows in THJobs
for (i in 1:nrow(jobs)) { #for each company in jobs
  for (j in 1:nrow(trackedco)){ #for each company in trackedco
    if (gsub("[\r\n]", "", jobs[i,2]) #if the company in jobs matches...
        %in% trackedco[j,1]) { #...company in trackedco...
      THJobs[k,] <- rbind(c(gsub("[\r\n]", "", jobs[i,1]), #write info from jobs to new report THJobs
                            gsub("[\r\n]", "", jobs[i,2]), 
                            gsub("[\r\n]", "", jobs[i,3])))
      k = k+1
    }
  }
}

View(THJobs)

#export THJobs
write.csv(THJobs, file = "C:/Users/John Johnson/Desktop/THJobs.csv")
