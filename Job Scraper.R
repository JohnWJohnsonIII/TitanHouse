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
wurl <- read_html("https://www.indeed.com/jobs?as_and=&as_phr=&as_any=&as_not=&as_ttl=%22Vice+President%22+or+%22President%22+or+%22Chief%22&as_cmp=&jt=fulltime&st=&salary=&radius=25&l=&fromage=15&limit=50&sort=date&psf=advsrch")

#grabs all results links available from first page of results (first 1K jobs)
page.links <- wurl %>%
  html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
  html_attr('href')
page.links <- as.data.frame(page.links)

baseURL <- "http://www.indeed.com"


#create dataframe to hold results of url pasting
allURLS <- data.frame(FullURL=character(),
                      stringsAsFactors = FALSE)
colnames(allURLS) <- c("FullURL")

#loop appends baseURL to page.links
g=1 #set counter for rows in page.links
for (i in 2:nrow(page.links)) { #starts at 2 because one of the URLs is captured twice in page.links
  next_URL <- paste(baseURL, page.links[i,], sep="")
  allURLS[g,] <- next_URL
  g = g+1
}

#View(allURLS)
#View(allURLS)  #check your results from URL pasting loop

#scraping the first page of results
#and establishing df's to hold results
title <- wurl %>% 
  html_nodes(".jobtitle") %>%
  html_text(trim=TRUE) %>% 
  replace(!nzchar(.), NA)
title <- as.data.frame(title)
#print(title)

company <- wurl %>% 
  html_nodes(".company") %>%
  html_text(trim=TRUE) %>% 
  replace(!nzchar(.), NA)
company <- as.data.frame(company)
#print(company)

location <- wurl %>% 
  html_nodes(".location") %>%
  html_text(trim=TRUE) %>% 
  replace(!nzchar(.), NA)
location <- as.data.frame(location)
#print(location)


###loop scrapes through each subsequent URL 
###and writes results to data frames
for (i in 1:nrow(allURLS)) {
  x2 <- read_html(allURLS[i,]) %>% 
    html_nodes(".jobtitle") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  x2 <- as.data.frame(x2)
  #print(title)
  
  y2 <- read_html(allURLS[i,]) %>% 
    html_nodes(".company") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  y2 <- as.data.frame(y2)
  #print(company)
  
  z2 <- read_html(allURLS[i,]) %>% 
    html_nodes(".location") %>%
    html_text(trim=TRUE) %>% 
    replace(!nzchar(.), NA)
  z2 <- as.data.frame(z2)

  title <- rbind.fill(title, x2)
  company <- rbind.fill(company, y2)
  location <- rbind.fill(location, z2)
}

###at this point all data points from all listings should be compiled
###into the title, company, and location df's

#write results to a single dataframe
jobs <- data.frame(title, company, location)


View(jobs)


####compare companies in jobs df to TH-tracked companies list

#read tracked companies list
trackedco <- read.csv("C:/Users/John/Desktop/TH tracked co's .csv", header = TRUE)
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
        %in% trackedco[j,1]) { #...company in trackedco
      THJobs[k,] <- rbind(c(gsub("[\r\n]", "", jobs[i,1]), #write info from jobs to new report THJobs
                            gsub("[\r\n]", "", jobs[i,2]), 
                            gsub("[\r\n]", "", jobs[i,3])))
      k = k+1
    }
  }
}

View(THJobs)

#export THJobs
write.csv(THJobs, file = "C:/Users/John/Desktop/THJobs.csv")
