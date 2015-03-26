require(data.table)

################################################################################
read.logs.env <- new.env()

################################################################################
# static functions

# Get the date part from a filename like "statistics.log.2011-10-01".
date.filename <- function(filename) {
        strsplit(filename, split = "[.]")[[1]][[3]]   
}

################################################################################
# local varibles
local({
        # The path to a temporary password file
        TMPPWF <- "~/tmppw.sh"
        
        eventLevels <- c(1,2,4,5,7,8,9,10,12,13,14,15,16)
        roleLevels <- c(0,1,2)
        
        stats_events <- NULL
        serveraddress <- NULL
}, env = read.logs.env)

#########################################
.load.all <- local(function() {
        # message("Loading reference tables.")
        stats_events <<- read.csv("ref/stats_events.csv")
        serveraddress <<- readLines("serveraddress.txt", n = 1)
}, env = read.logs.env)

.load.all()

#########################################
.handleData <- local(function(t, filename, data) {
       # interface function 
}, env = read.logs.env)

#########################################
#          general functions            #

# Read remote log files.
# You need to set your password for the server in /usr/libexec/ssh-askpass:
#---------------------------------------
#       #!/bin/bash --
#       
#       echo "password"
#---------------------------------------
#
# Alternatively use the interactive functions for setting password in utils.R.
# First redirect from /usr/libexec/ssh-askpass to a file in a place where we 
# can write. Create the file /usr/libexec/ssh-askpass...
#---------------------------------------
#       #!/bin/bash --
#       
#       ~/tmppw.sh
#---------------------------------------
# ... and chmod to executable. 
# See functions .create_pwf and .destroy_pwf

# Read the logs.
#
#' @param max: integer. maximum amount of files to read. Negative values for all files.
#' @param fun: a function to handle the data from a file just read.
#' @param filter: a function that filters files.
remote.read.logs <- function(max = 5, fun = .handleData, filter = filter.filename, 
                     despwf = TRUE, progress = TRUE, ...) {
        .create_pwf()
        
        n <- max
        ls <- remote.ls(filter, despwf = FALSE)
        tt <- length(ls)
        if (max > 0) tt <- max
        t <- 0
        message("Reading ", tt, " log files from a total of ", length(ls))
        for (filename in ls) {
                data <- remote.read.data(filename, despwf = FALSE)
                t <- t + 1
                tryCatch({
                        fun(t, filename, data)  
                }, warning = function(w) {
                        message(w, "\n\tt=", t, " filename=", filename, "\n")
                }, error = function(e) {
                        message(e, "\n\tt=", t, " filename=", filename, "\n")
                })
                
                
                if (progress) {
                        mes <- paste(".", format((t/tt) * 100, digits = 0), "%", paste(" (", t, "/", tt, ")", sep = ""), sep = "")
                        bes <- as.character(paste(rep("\b", nchar(mes) - 1), collapse = "", sep=""))
                        cat(bes, mes, sep = "")
                }
                
                if (n > 0) n <- n - 1
                if (n == 0) break
        }
        .destroy_pwf(despwf)
}

#########################################
# Read a single log file.
#
remote.read.data <- local(function(filename, despwf = TRUE) {
        if (substr(filename, 1, 15) != "statistics.log.") {
                filename <- paste("statistics.log.", filename, sep = "")
        }
        .create_pwf()
        
        # read the serveraddress
        serveraddress <- readLines("serveraddress.txt", n = 1)
        
        com <- paste("ssh ", serveraddress ," 'cat /data/statsdata/logs/", filename, "'", sep = "")
        pip <- pipe(com)
        data <- readLines(pip, skipNul = TRUE)
        close(pip)
        .destroy_pwf(despwf)
        data
}, env = read.logs.env)

#########################################
# Read a single log file and save it locally.
#
remote.save.data <- local(function(filename, despwf = TRUE) {
        if (substr(filename, 1, 15) != "statistics.log.") {
                filename <- paste("statistics.log.", filename, sep = "")
        }
        if (!file.exists("logs")) {
                dir.create("logs")
        }
        path <- file.path("logs", filename)
        writeLines(remote.read.data(filename, despwf), path)
}, env = read.logs.env)

#########################################
# List files in the directory /data/statsdata/logs on talfalab11.
#
#' @param filter a function that filters files on filename.
remote.ls <- local(function(filter = NULL, despwf = TRUE) {
        .create_pwf()
        pip <- pipe(paste0('ssh ', serveraddress, ' "ls -1 /data/statsdata/logs"'))
        ls <- readLines(pip)
        close(pip)
        .destroy_pwf(despwf)
        if (!is.null(filter)) {
                ls <- Filter(filter, ls) 
        }       
        ls
}, env = read.logs.env)

########################################
# Cleaning data

# Read data of one log file into a data.table with 5 common fields:
# "date_time", "event_id", "user", "role", "ip_address".
.cd.commonDT <- local(function(data) {
        df <- as.data.frame(.cd.commonMat(data), stringsAsFactors = FALSE)
        colnames(df) <- c("date_time", "event_id", "user", "role", "ip_address")
        df$date_time <- as.POSIXct(df$date_time)
        df$event_id <- factor(as.integer(df$event_id), levels = eventLevels)
        df$user <- as.factor(df$user)
        df$role <- factor(as.integer(df$role), levels = roleLevels)
        df$ip_address <- as.factor(df$ip_address)
        
        #df$agent1 <- as.factor(bot.lookup1(as.character(df$ip_address)))
        #df$agent2 <- as.factor(bot.lookup2(as.character(df$ip_address)))
        
        #dt <- as.data.table(df)
        #dt[, agent1:=as.factor(bot.lookup1(as.character(ip_address)))]
        #dt[, agent2:=as.factor(bot.lookup2(as.character(ip_address)))]
        as.data.table(df)
}, env = read.logs.env)

# Read in the common fields into a matrix 5 fields wide. See commonDf.
.cd.commonMat <- function(data) {
        lines <- Filter(.is.date_start, data)
        splines <- sapply(lines, .cd.commonFields, USE.NAMES = FALSE)
        mat <- matrix(splines, ncol = 5, byrow = TRUE)
        mat <- cbind(mat, sapply(strsplit(mat[, 1],' - '), "[", 2)) # events on 6
        mat[, 6] <- sapply(mat[, 6], .cd.eventLookup)
        mat[, 1] <- sapply(strsplit(mat[, 1],' - '), "[", 1) # dates
        mat[, 3] <- grepl("USER", mat[, 3]) + grepl("ARCHIVIST", mat[, 3])
        mat[, 5] <- ifelse(grepl("0:0:0:0:0:0:0:1", mat[, 5]), "127.0.0.1", mat[, 5])
        mat[, 5] <- ifelse(grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", mat[, 5]), mat[, 5], NA)
        mat[, c(1, 6, 2, 3, 5)]
}

.cd.commonFields <- function(line) {
        strsplit(line, split = " ; ")[[1]][1:5]
}

.cd.eventLookup <- local(function(x) {
        id <- stats_events[stats_events$event == x, 1]
        if (length(id) == 0) return(NA)
        id
}, env = read.logs.env)

#########################################
# Read the common fields of one log file in a data.table
#
remote.data.table <- function(filename, despwf = TRUE) {
        .cd.commonDT(remote.read.data(filename, despwf))
}

################################################################################
# print a summary for a certain date
getSummary <- function(filename) {
        dat <- remote.read.data(filename)
        dt <- .cd.commonDT(dat)
        print(summary(dt))
        dt
}

getClientSummary <- function(filename, agentname = NULL) {
        dt <- getSummary(filename)
        if (is.null(agentname)) {
                agentname <- names(summary(dt$agent1))[1]
        }
        dtc <- dt[dt$agent1 == agentname, ]
        print(summary(dtc))
        dt
}

#########################################
#        file filter functions          #
filter.filename <- function(filename) {
        pass <- FALSE
        if (substr(filename, 1, 15) == "statistics.log.") pass <- TRUE  
        pass
}

filter.research.period <- function(filename, 
                                 start = as.Date("2011-10-01"),
                                 end = as.Date("2014-09-30")) {
        pass <- FALSE
        if (filter.filename(filename)) {
                date <- as.Date(date.filename(filename))
                pass <- date >= start & date <= end
        }
        pass
}

#########################################
# filter log statements on client-ip
# cipa <<- client-ip
.is.client_ip <- function(line) {
        strsplit(line, split = " ; ")[[1]][5] == cipa 
}

.is.date_start <- function(line) {
        # 2013-07-14 12:14:12,389
        ds <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", line)
        if (!ds) {
                warning("\nLine does not start with date: \n", line)   
        }
        ds
}

#########################################
#  filter log statements on event       
.is.event <- function(line, action) {
        strsplit(strsplit(line, split = " ; ")[[1]][1], " - ")[[1]][2] == action 
}

.is.ADVANCED_SEARCH_TERM <- function(line) {
        .is.event(line, "ADVANCED_SEARCH_TERM")
}

.is.START_PAGE_VISIT <- function(line) {
        .is.event(line, "START_PAGE_VISIT")
}

.is.USER_URL_REQUEST <- function(line) {
        .is.event(line, "USER_URL_REQUEST")
}

.is.USER_REGISTRATION <- function(line) {
        .is.event(line, "USER_REGISTRATION")
}

.is.USER_LOGIN <- function(line) {
        .is.event(line, "USER_LOGIN")
}

.is.USER_LOGOUT <- function(line) {
        .is.event(line, "USER_LOGOUT")
}

.is.DATASET_VIEWED <- function(line) {
        .is.event(line, "DATASET_VIEWED")
}

.is.SEARCH_TERM <- function(line) {
        .is.event(line, "SEARCH_TERM")
}

.is.DOWNLOAD_DATASET_REQUEST <- function(line) {
        .is.event(line, "DOWNLOAD_DATASET_REQUEST")
}

.is.DOWNLOAD_FILE_REQUEST <- function(line) {
        is.event(line, "DOWNLOAD_FILE_REQUEST")
}

.is.DATASET_DEPOSIT <- function(line) {
        .is.event(line, "DATASET_DEPOSIT")
}

.is.FILE_DEPOSIT <- function(line) {
        .is.event(line, "FILE_DEPOSIT")
}

.is.DATASET_PUBLISHED <- function(line) {
        .is.event(line, "DATASET_PUBLISHED")
}

#########################################
# Functions to work with temporary password files for non-interactive ssh.

# If TMPPWF does not exist, ask for non-interactive ssh password file
# and create TMPPWF.
.create_pwf <- local(function() {
        if (!file.exists(TMPPWF)) {
                pass <- .rs.askForPassword("Enter password for server:")
                if (is.null(pass)) {
                        message("User aborted action.")
                        return(NULL)
                }
                writeLines(paste("#!/bin/bash --\n\necho \"", pass, "\"", sep = ""), TMPPWF)
                Sys.chmod(TMPPWF)
        }
}, env = read.logs.env)

# Remove TMPPWF if parameter despwf is TRUE.
.destroy_pwf <- local(function(despwf) {
        if (file.exists(TMPPWF) & despwf) {
                file.remove(TMPPWF)
        }
}, env = read.logs.env)

#########################################

################################################################################

rm(read.logs.env)

################################################################################
