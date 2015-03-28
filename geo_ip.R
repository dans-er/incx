# Keeps a table with geographic information on ip-addresses.
# If an ip-address is not available will lookup and add to table

source("bot_collector.R")
require(data.table)
require(rjson)

filename <- "ref/geoip.csv"

.lookup.geo.ip <- function(ips) {
        
        ip0 <- Filter(ip.is.not.private, as.character(ips))
        
        dt.geo <- data.table()
        t <- 1
        for (ip in ip0) {
                if (!ip.is.ip4(ip))
                {
                        message("Illegal ip address ", ip)
                } else {
                        url <- paste0("http://www.telize.com/geoip/", ip)
                        message(t, url)
                        download.file(url, "ref/geoip.json", "curl", quiet = TRUE)
                        x <- fromJSON(file = "ref/geoip.json", method = "C") # returns a lis
                        dt.geo <- rbind(dt.geo, x, use.names=TRUE, fill=TRUE)
                        t <- t + 1
                }
        }
        dt.geo
}

.do.lookup.geo.ip <- function(abips, dt.geo = NULL) {
        # telize.com does not answer if we send too many requests
        step <- 20; pause <- 5
        for (i in seq(1, length(abips), step)) {              
                dt.abs <- .lookup.geo.ip(abips[i:(min(i+step-1, length(abips)))])
                message("Collected new geographical information on ", nrow(dt.abs), " IP-addresses")
                if (nrow(dt.abs) > 0) {
                        if (is.null(dt.geo)) {
                                dt.geo <- dt.abs
                        } else {
                                dt.geo <- rbind(dt.geo, dt.abs, fill = TRUE)                                
                        }
                        write.csv(dt.geo, filename, row.names = FALSE)
                        Sys.sleep(pause)
                }
        }
}

.read.geo.ip.table <- function(ips) {
        
        if (!file.exists("ref")) {
                dir.create("ref")
        }
        if (!file.exists(filename)) {
                .do.lookup.geo.ip(ips)
        }
        dt.geo <- data.table(read.csv(filename, stringsAsFactors = FALSE))
        
        # filter out private IP-addresses
        ip0 <- Filter(ip.is.not.private, as.character(ips))
        
        # look up absent IP-addresses
        abips <- ip0[!(ip0 %in% dt.geo$ip)]
        message("there are ", length(abips), " absent ip-addresses")
        if (length(abips) > 0) {
                .do.lookup.geo.ip(ips, dt.geo) 
                dt.geo <- data.table(read.csv(filename, stringsAsFactors = FALSE))
        }
        setkey(dt.geo, "ip")
        dt.geo
}

geo.info <- function(ips) {
        # get a table of ip geo info for ips 
        
        ip0 <- ips[!is.na(ips)]
        ip0 <- Filter(ip.is.not.private, as.character(ip0))
        dt.geo <- .read.geo.ip.table(ip0)
        dt.gip <- dt.geo[ip %in% ip0]
        setnames(dt.gip, old = "ip", new = "ip_address")
        setkey(dt.gip, "ip_address")
        dt.gip
}

