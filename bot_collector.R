
################################################################################
# get some usefull data
bot.collector.env <- new.env()

################################################################################
# static functions
ip.is.ip4 <- function(line) {
        # 4 groups of 1-3 numbers divided by .
        grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", line)
}

.is.ip3 <- function(line) {
        # 3 groups of 1-3 numbers divided by .
        grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", line)
}

.is.ip2 <- function(line) {
        # 2 groups of 1-3 numbers divided by .
        grepl("^[0-9]{1,3}\\.[0-9]{1,3}$", line)
}

#-------------------------------------------------------------------------------

# returns an integer vector for ip-addresses.
# ips - character vector of ip-addresses.
# no.ip - value to substitute for items that are not ip4.
ip.as.integer <- function(ips, no.ip = -1) {
        rips <- ifelse(ip.is.ip4(ips), ips, "0.0.0.0")
        vals <- read.table(text=as.character(rips), sep=".")
        ints <- ((vals[4] + 256*vals[3] + 256^2*vals[2] + 256^3*vals[1])[,1])
        ifelse(ints == 0, no.ip, ints)
}

################################################################################
# local variables
local({
        require(data.table)
        require(XML)
        require(lubridate)
        
        
        dataDir <- "data"
        dataDirBotIP <- file.path(dataDir, "bot_ip")
        
        reloadLists <- FALSE
        dataIPlists <- file.path(dataDirBotIP, "iplists.csv")
        dataDirIPlists <- file.path(dataDirBotIP, "lists")
        data4IPs <- file.path(dataDirBotIP, "ip4.csv")
        data3IPs <- file.path(dataDirBotIP, "ip3.csv")
        
        iplistsURL <- "http://www.iplists.com/nw/"
        
        dt.ip3 <- NULL
        dt.ip4 <- NULL
        dt.index <- NULL
        dt.bot <- NULL
}, env = bot.collector.env)

################################################################################
.getIndexIPlists <- local(function() {
        list <- list()
        at <- 1
        st <- 1
        
        htmlTreeParse(iplistsURL, useInternalNodes = TRUE, 
        handlers = c(
        a = function(node) {
                # <a class="iplist" href="google.txt">Google IP List</a>
                class <- xmlGetAttr(node, "class")
                if (length(class) > 0 && class  == "iplist") {
                        if (st != at) {
                                stop("a: Missing element. a=", at, " span=", st)
                        }
                        list[["name"]][at] <<- xmlValue(node)
                        list[["filename"]][at] <<- xmlGetAttr(node, "href")
                        at <<- at + 1
                        #print(node)
                }
        },
        span = function(node) {
                # <span class="age">List of the IP addresses of the various Googlebots and Mediabots. Last revised Sunday, 26-Jan-2014 10:59:44 EST  Size: 4.3K</span><br />
                class <- xmlGetAttr(node, "class")
                if (length(class) > 0 && class == "age") {
                        val <- xmlValue(node)
                        list[["description"]][st] <<- gsub("\\..*", "", val) # up to first.
                        
                        pos <- regexec("[0-9]{1,2}-[A-Z]{1}[a-z]{2}-[0-9]{4}", val)
                        if (pos[[1]][1] > 0) {
                                datestring <- regmatches(val, pos)[[1]]
                                list[["revision"]][st] <<- as.character(dmy(datestring))
                        } else {
                                list[["revision"]][st] <<- NA
                        }
                        st <<- st + 1
                        if (st != at) {
                                stop("span: Missing element. a=", at, " span=", st)
                        }
                        #print(node)
                }
        }))
        df.index <- as.data.frame(list)
        write.csv(df.index, dataIPlists, row.names = FALSE)
        message("Saved '", dataIPlists, "' rows: ", nrow(df.index), ", cols: ", ncol(df.index))
        df.index
}, env = bot.collector.env)

.downloadIPlists <- local(function() {
        if (!file.exists(dataDirIPlists)) dir.create(dataDirIPlists)
        df.index <- read.csv(dataIPlists, stringsAsFactor = FALSE)
        vdate <- character()
        vurl <- character()
        for (filename in df.index$filename) {
                url <- paste0(iplistsURL, filename)
                download.file(url, 
                              file.path(dataDirIPlists, filename),
                              method = "curl",
                              quiet = TRUE)
                message("Downloaded ", url)
                vdate <- c(vdate, format(Sys.Date(), "%Y-%m-%d"))
                vurl <- c(vurl, url)
        }
        df.index$downloaded <- vdate
        df.index$url <- vurl
        write.csv(df.index, dataIPlists, row.names = FALSE)
}, env = bot.collector.env)

.readIPlists <- local(function() {
        df.index <- read.csv(dataIPlists, stringsAsFactor = FALSE)
        v4ip <- character()
        v4ag <- character()
        v3ip <- character()
        v3ag <- character()
        vindex4 <- integer()
        vindex3 <- integer()
        for (filename in df.index$filename) {
              file <- file.path(dataDirIPlists, filename)
              name <- gsub("\\..*", "", filename) # strip .txt
              data <- readLines(file)
              
              # 4-group ip-addresses
              ips <- Filter(ip.is.ip4, data)
              n <- length(ips)
              v4ip <- c(v4ip, ips)
              v4ag <- c(v4ag, rep(name, n))
              vindex4 <- c(vindex4, n)
              
              # 3-group ip-addresses
              ips <- Filter(.is.ip3, data)
              n <- length(ips)
              v3ip <- c(v3ip, ips)
              v3ag <- c(v3ag, rep(name, n))
              vindex3 <- c(vindex3, n)
              
              # 2-group ip-addresses
              ips <- Filter(.is.ip2, data)
              if (length(ips) > 0) {
                    warning("Found 2-group ip-addresses: ", file)  
              }
              
              
        }
        df.4ip <- data.frame(IP = v4ip, agent = v4ag, stringsAsFactors = FALSE)
        df.3ip <- data.frame(IP = v3ip, agent = v3ag, stringsAsFactors = FALSE)
        
        write.csv(df.4ip, data4IPs, row.names = FALSE)
        write.csv(df.3ip, data3IPs, row.names = FALSE)
        
        message("Saved '", data4IPs, "' rows: ", nrow(df.4ip))
        message("Saved '", data3IPs, "' rows: ", nrow(df.3ip))
        
        # number of ip4 and ip3 addresses to index
        df.index$ip4 <- vindex4
        df.index$ip3 <- vindex3
        write.csv(df.index, dataIPlists, row.names = FALSE)
}, env = bot.collector.env)

################################################################################

.checkData <- local(function(){
        if (!file.exists(dataDir)) dir.create(dataDir)
        if (!file.exists(dataDirBotIP)) dir.create(dataDirBotIP)
        
        if (reloadLists | !file.exists(dataIPlists)) {
                .getIndexIPlists()
                .downloadIPlists()
                .readIPlists()
        }
}, env = bot.collector.env)

.checkData()

################################################################################

.load.all <- local(function() {
        dt.index <<- as.data.table(read.csv(dataIPlists, stringsAsFactors = FALSE))
        dt.ip4 <<- as.data.table(read.csv(data4IPs, stringsAsFactors = FALSE))
        dt.ip3 <<- as.data.table(read.csv(data3IPs, stringsAsFactors = FALSE))
        
        dt.bot <<- as.data.table(read.table("data/bot_ip/bot_ip.txt", stringsAsFactors = FALSE))
        dt.bot$from <<- ip.as.integer(dt.bot$from) 
        dt.bot$to <<- ip.as.integer(dt.bot$to)
}, env = bot.collector.env)

.load.all()
################################################################################


################################################################################
# lookup lists from "http://www.iplists.com/nw/"
bot.lookup1 <- local(function(ips) {
        ip0 <- as.character(ips)
        b <- dt.ip4$agent[match(ip0, dt.ip4$IP)]
        ips4 <- ifelse(is.na(b), "normal", b)
        
        b <- dt.ip3$agent[match(gsub("\\.[0-9]{1,3}$", "", ip0), dt.ip3$IP)]
        ifelse(is.na(b), "normal", b)
}, env = bot.collector.env)

bot.is.bot <- local(function(ips) {
        ips %in% dt.ip4$IP | gsub("\\.[0-9]{1,3}$", "", ips) %in% dt.ip3$IP
}, env = bot.collector.env)


bot.names <- local(function() {
        gsub("\\..*", "", list.files(dataDirIPlists, pattern = ".*\\.txt$")) # strip .txt
}, env = bot.collector.env)

bot.desc <- local(function(name = NULL) {
        if(is.null(name)) {
                dt.index[, -3, with = FALSE]
        } else {
                dt.index[filename == paste0(name, ".txt"), -3, with = FALSE]
        }
}, env = bot.collector.env)

.bot.table <- local(function(ip = 4) {
        if (ip == 3) {
                return(dt.ip3)
        } else {
                return(dt.ip4)
        }
}, env = bot.collector.env)

################################################################################
# lookup with bot_ip.txt, robots ip address ranges from http://chceme.info/ips/
bot.lookup2 <- local(function(ips) {
        cips <- ip.as.integer(as.character(ips)) # -1 for non-ip4
        rips <- sapply(cips, .is_bot, USE.NAMES = FALSE)
        #ifelse(rips == "no.bot", ips, rips)
        ifelse(rips == "no.bot", "normal", rips)
}, env = bot.collector.env)

.is_bot <- local(function(cip) {
        rip <- "no.bot"
        x <- cip >= dt.bot$from & cip <= dt.bot$to
        if (sum(x) >= 1) {
                rip <- dt.bot$agent[x]
        }
        rip
}, env = bot.collector.env)

ip.is.private <- function(ips) {
        bips <- bot.lookup2(ips)
        grepl("^Private.*", bips)
}

ip.is.not.private <- function(ips) {
        !ip.is.private(ips)
}

################################################################################

ip.agent <- function(ips) {
      bots1 <- bot.lookup1(ips) 
      bots2 <- bot.lookup2(ips)
      ifelse(bots1 != "normal", tolower(bots1), 
             ifelse(bots2 != "normal", tolower(bots2), "normal"))
}

################################################################################
rm(bot.collector.env)

################################################################################



