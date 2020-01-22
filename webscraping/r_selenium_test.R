library(RSelenium)

rD<-rsDriver(browser = c("chrome"))
driver<-rD$client

driver$navigate("http://www.wikipedia.com")

library(httr)
library(rvest)
read_html("http://books.toscrape.com/")
driver$closeall()
rD$server$log()
rD$server$process$c_handle
rD$server$process$command
rD$server$process$arguments
rD$server$output()
rD$server$stop()
