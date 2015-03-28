################################################################################
## global_options
source("readlogs.R")
source("bot_collector.R")
source("geo_ip.R")

require(data.table)
require(reshape2)

################################################################################
## load_data /cached
if (!exists("dt.log")) {
        yesterday <- Sys.Date() - 1
        yesterfor <- format(yesterday, "%A %d-%m-%Y")
        dt.log <- remote.data.table(yesterday)
        dt.log[, agent:=ip.agent(ip_address)]
        dt.events <- data.table(read.csv("ref/stats_events.csv", stringsAsFactors = FALSE))
        setkey(dt.events, "id")
}

### Geographic spread of client IP-addresses
################################################################################
## geo_spread
## get the unique IP-addresses
dt.ips <- dt.log[, j=list(frequency = .N), by = c("ip_address")]
dt.ips[, agent:=ip.agent(ip_address)]
setkey(dt.ips, "ip_address")
# 
## get a table with geo information on the addresses
dt.gip <- geo.info(dt.ips$ip_address)
dt.ll <- dt.gip[!(is.na(longitude) | is.na(latitude)),]
dt.ll <- merge(dt.ll, dt.ips, by = "ip_address")

require(maps)
require(mapdata)

agents <- levels(factor(dt.ll$agent))
cols <- rainbow(length(agents), start=0.3, alpha=0.3)
sats <- rainbow(length(agents), start=0.3, alpha=1)
dt.col <- data.table(agent = agents, color = cols, sat = sats)

dt.world <- merge(dt.ll, dt.col, by = "agent")
max.color <- dt.world[which.max(frequency),]$sat
max.agent <- dt.world[which.max(frequency),]$agent
max.frequency <- dt.world[which.max(frequency),]$frequency
# 
# 
# map('worldHires')
# points(x = dt.world$longitude, y = dt.world$latitude, col=dt.world$color, 
#        pch=19, cex = log10(dt.world$frequency) * 3)
# # legend point
# rect(xleft = -180, xright = -50, ybottom = -90, ytop = -60, col = "white", border = "white")
# points(x = -160, y = -70, pch=21, cex = log10(1000) * 3, 
#        col = max.color, bg="white")
# points(x = -160, y = -70, pch=21, cex = log10(100) * 3, 
#        col = max.color, bg="white")
# points(x = -160, y = -70, pch=21, cex = log10(10) * 3, 
#        col = max.color, bg="white")
# 
# text(x = -147, y = -80, labels="number of requests per IP-address:\n1000, 100, 10 requests", 
#      cex = 0.8, pos = 4)
# 
# legend(x = "bottomright", legend = dt.col$agent, col=dt.col$sat, 
#        pch = 19, cex = 0.8, horiz=TRUE, bg = "white")
# title(main = paste("Geographic spread of requests ~", yesterday))

### European spread of client IP-addresses
################################################################################
## eu_spread
xl <- -2; xr <- 11; yb <- 49; yt <- 54
dt.eu <- dt.world[longitude >= xl & longitude <= xr & latitude >= yb & latitude <= yt]

map('worldHires', xlim=c(xl, xr), ylim=c(yb, yt))
points(x = dt.eu$longitude, y = dt.eu$latitude, col=dt.eu$color, 
       pch=19, cex = log10(dt.eu$frequency) * 3)
title(main = paste("Spread of requests, neighbouring countries, ", yesterfor), 
      cex.main = 0.95)

# ## nl_spread
# xl <- 3.3; xr <- 7.25; yb <- 50.75; yt <- 53.49
# dt.nl <- dt.world[longitude >= xl & longitude <= xr & latitude >= yb & latitude <= yt]
# 
# map('worldHires', xlim=c(xl, xr), ylim=c(yb, yt))
# points(x = dt.nl$longitude, y = dt.nl$latitude, col=dt.nl$color, 
#        pch=19, cex = log10(dt.nl$frequency) * 3)
# title(main = paste("Spread of requests, Netherlands, ", yesterfor), 
#       cex.main = 0.95)

# ### Summary of events
# ################################################################################
# ## event_frequency
# ## counts per event_id, agent
# dt.fat <- dt.log[, j=list(frequency = .N), by = c("event_id", "agent")]
# dt.fat.wide <- dcast(dt.fat, event_id ~ agent, value.var = "frequency", fill = "")
# setnames(dt.fat.wide, old = "event_id", new = "id")
# ## counts per event
# 
# dt.counts <- dt.log[, j=list(frequency = .N), by = c("event_id")]
# setnames(dt.counts, old = "event_id", new = "id")
# 
# ## merges with event description and split-counts per agent type
# dt.counts <- merge(dt.counts, dt.events[, c(1, 4), with = FALSE], by = "id", all.x = TRUE)
# dt.counts <- merge(dt.counts, dt.fat.wide, by = "id", all.x = TRUE)
# align <- rep("r", ncol(dt.counts) - 1)
# align[2] <- "l"


