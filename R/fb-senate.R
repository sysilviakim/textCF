# Basics =======================================================================

## Download and load any necessary packages
# install.packages("devtools")
# devtools::install_github("facebookresearch/Radlibrary")
# library(Radlibrary)

# Setup ========================================================================

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token
# will expire within an hour(?) of generation, so be mindful of that.
# For me, it is no more than an hour
token <- readline()

# Data Wrangling ===============================================================

fb_senate <- read.csv("data/raw/fb/fb-senate.csv")
senatedat <- as.data.frame(cbind(
  fb_senate$candidate, fb_senate$state,
  fb_senate$fb_ad_library_id
))
colnames(senatedat) <- c("candidate", "state", "id")
senatedat$id <- as.numeric(senatedat$id)
senatedat <- senatedat[!is.na(senatedat$id), ] # Takes us from 75 to 72
senateframes <- split(senatedat, rep(1:8, each = 10))
# We splilt the data into frames with a maximum of 10 members per frame
# because we can only query the ads for ten pages at a time.


senate1ids <- as.vector(senateframes[["1"]]$id)

sen.query1 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = senate1ids,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
# This query will pull all of the ads for the first ten senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

sen.response1 <- adlib_get(params = sen.query1, token = token)
sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# An issue has arisen. The query has only pulled the last 5,000 observations,
# not the 10,000 requested. Perhaps this is the outright cap for queries?

# Perhaps splitting further, or else having multiple queries for each group
# arranging by date? I think splitting further would run the lowest amount of
# risk...we don't want duplicated ads in our final sample.

# Further splitting ============================================================

# senateframes2 <- split(senatedat,rep(1:15,each=5))
# We splilt the data into frames with a maximum of 5 members per frame this time

# senateidsA <- as.vector(senateframes2[["1"]]$id)

# sen.query1 <- adlib_build_query(ad_reached_countries = 'US',
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsA,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first 5 senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

# sen.response1 <- adlib_get(params = sen.query1, token = token)
# sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# Even here, we hit the 5,000 cap in July - better than September, but not great

# senateframes3 <- split(senatedat,rep(1:24,each=3))

# senateidsB <- as.vector(senateframes3[["1"]]$id)

# sen.query1 <- adlib_build_query(ad_reached_countries = 'US',
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsB,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first 3 senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

# sen.response1 <- adlib_get(params = sen.query1, token = token)
# sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# Hopefully this will keep working for the remaining 23 frames

# Frame 2:
## >5,000 - would have to break it up into multiple groups...
# senateidsB2 <- as.vector(senateframes3[["2"]]$id)
# sen.query2 <- adlib_build_query(ad_reached_countries = 'US',
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsB2,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# sen.response2 <- adlib_get(params = sen.query2, token = token)
# sen.tibble2 <- as_tibble(sen.response2, type = "ad")

# Ultimately, what makes the most sense, I think, will be to use the blocks of
# ten candidates apiece, run tests, find the last date that ads were pulled on,
# and use that date as the min date for the first round and the max for the
# next round

# Ultimate Approach ============================================================

# Repeated from earlier - I intend to cut out the extraneous stuff, and replace
# it with comments about why I'm doing this approach, at the end

# senateframes <- split(senatedat,rep(1:8,each=10))
# We splilt the data into frames with a maximum of 10 members per frame
# because we can only query the ads for ten pages at a time.


# senate1ids <- as.vector(senateframes[["1"]]$id)

# sen.query1 <- adlib_build_query(ad_reached_countries = 'US',
#                                ad_active_status = 'ALL',
#                                search_page_ids = senate1ids,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first ten senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

# sen.response1 <- adlib_get(params = sen.query1, token = token)
# sen.tibble1 <- as_tibble(sen.response1, type = "ad")

## Group 1 =====================================================================

# senate.1.ids <- as.vector(senateframes[["1"]]$id)

# sen.query.1a <- adlib_build_query(ad_reached_countries = 'US',
#                                ad_active_status = 'ALL',
#                                search_page_ids = senate.1.ids,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-09-25',
#                                limit = 5000,
#                                fields = "ad_data")
# This is by delivery date, which makes it even more difficult to parse out
# what to do...
## I thought this approach would work better, but it seems I was wrong

# sen.response.1a <- adlib_get(params = sen.query.1a, token = token)
# sen.tibble.1a <- as_tibble(sen.response.1a, type = "ad")

# New approach: Individual pulls ===============================================

## 1. Doug Jones, AL ===========================================================

jones.al <- as.vector(senatedat[1, "id"])
jones.al.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = jones.al,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
jones.response <- adlib_get(params = jones.al.query, token = token)
jones.tibble <- as_tibble(jones.response, type = "ad")
jones.tibble$candidate <- "Doug Jones, AL"

## 2. Tommy Tuberville, AL =====================================================

tuberville <- as.vector(senatedat[2, "id"])
tuberville.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = tuberville,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
tuberville.response <- adlib_get(params = tuberville.query, token = token)
tuberville.tibble <- as_tibble(tuberville.response, type = "ad")
tuberville.tibble$candidate <- "Tommy Tuberville, AL"

fb.senate <- rbind(jones.tibble, tuberville.tibble)

## 3. Al Gross, AK =============================================================

gross <- as.vector(senatedat[3, "id"])
gross.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = gross,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
gross.response <- adlib_get(params = gross.query, token = token)
gross.tibble <- as_tibble(gross.response, type = "ad")
gross.tibble$candidate <- "Al Gross, AK"

fb.senate <- rbind(fb.senate, gross.tibble)

## 4. Dan Sullivan, AK =========================================================

sullivan <- as.vector(senatedat[4, "id"])
sullivan.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = sullivan,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
sullivan.response <- adlib_get(params = sullivan.query, token = token)
sullivan.tibble <- as_tibble(sullivan.response, type = "ad")
sullivan.tibble$candidate <- "Dan Sullivan, AK"

fb.senate <- rbind(fb.senate, sullivan.tibble)

## 5. Martha McSally, AZ =======================================================

mcsally <- as.vector(senatedat[5, "id"])
mcsally.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = mcsally,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
mcsally.response <- adlib_get(params = mcsally.query, token = token)
mcsally.tibble <- as_tibble(mcsally.response, type = "ad")
mcsally.tibble$candidate <- "Martha McSally, AZ"

fb.senate <- rbind(fb.senate, mcsally.tibble)

## 6. Mark Kelly, AZ =======================================================

kelly <- as.vector(senatedat[6, "id"])
kelly.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = kelly,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
kelly.response <- adlib_get(params = kelly.query, token = token)
kelly.tibble <- as_tibble(kelly.response, type = "ad")
kelly.tibble$candidate <- "Mark Kelly, AZ"

fb.senate <- rbind(fb.senate, kelly.tibble)

# Kelly goes over 5,000 - will have duplicates
kelly.query.2 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = kelly,
  ad_delivery_date_max = "2020-02-01",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
kelly.response.2 <- adlib_get(params = kelly.query.2, token = token)
kelly.tibble.2 <- as_tibble(kelly.response.2, type = "ad")
kelly.tibble.2$candidate <- "Mark Kelly, AZ"

fb.senate <- rbind(fb.senate, kelly.tibble.2)

## 7. Tom Cotton, AR ===========================================================

cotton <- as.vector(senatedat[7, "id"])
cotton.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = cotton,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
cotton.response <- adlib_get(params = cotton.query, token = token)
cotton.tibble <- as_tibble(cotton.response, type = "ad")
cotton.tibble$candidate <- "Tom Cotton, AR"

fb.senate <- rbind(fb.senate, cotton.tibble)

## 8. John Hickenlooper, CO ====================================================

hickenlooper <- as.vector(senatedat[8, "id"])
hickenlooper.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = hickenlooper,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
hickenlooper.response <- adlib_get(params = hickenlooper.query, token = token)
hickenlooper.tibble <- as_tibble(hickenlooper.response, type = "ad")
hickenlooper.tibble$candidate <- "John Hickenlooper, CO"

fb.senate <- rbind(fb.senate, hickenlooper.tibble)

# Hickenlooper has > 5,000 - same schtick with him as Kelly
hickenlooper.query.2 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = hickenlooper,
  ad_delivery_date_max = "2020-04-30",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
hickenlooper.response.2 <- adlib_get(
  params = hickenlooper.query.2,
  token = token
)
hickenlooper.tibble.2 <- as_tibble(hickenlooper.response.2, type = "ad")
hickenlooper.tibble.2$candidate <- "John Hickenlooper, CO"

fb.senate <- rbind(fb.senate, hickenlooper.tibble.2)

## 9. Cory Gardner, CO ====================================================

gardner <- as.vector(senatedat[9, "id"])
gardner.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = gardner,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
gardner.response <- adlib_get(params = gardner.query, token = token)
gardner.tibble <- as_tibble(gardner.response, type = "ad")
gardner.tibble$candidate <- "Cory Gardner, CO"

fb.senate <- rbind(fb.senate, gardner.tibble)

## 10. Christopher Coons, DE ===================================================

coons <- as.vector(senatedat[10, "id"])
coons.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = coons,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
coons.response <- adlib_get(params = coons.query, token = token)
coons.tibble <- as_tibble(coons.response, type = "ad")
coons.tibble$candidate <- "Christopher Coons, DE"

fb.senate <- rbind(fb.senate, coons.tibble)

## 11. Lauren Witzke, DE =======================================================

witzke <- as.vector(senatedat[11, "id"])
witzke.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = witzke,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
witzke.response <- adlib_get(params = witzke.query, token = token)
witzke.tibble <- as_tibble(witzke.response, type = "ad")
witzke.tibble$candidate <- "Lauren Witzke, DE"

fb.senate <- rbind(fb.senate, witzke.tibble)

## 12. David Perdue, GA ========================================================

perdue <- as.vector(senatedat[12, "id"])
perdue.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = perdue,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
perdue.response <- adlib_get(params = perdue.query, token = token)
perdue.tibble <- as_tibble(perdue.response, type = "ad")
perdue.tibble$candidate <- "David Perdue, GA"

fb.senate <- rbind(fb.senate, perdue.tibble)

## 13. Jon Ossoff, GA ========================================================

ossoff <- as.vector(senatedat[13, "id"])
ossoff.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = ossoff,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
ossoff.response <- adlib_get(params = ossoff.query, token = token)
ossoff.tibble <- as_tibble(ossoff.response, type = "ad")
ossoff.tibble$candidate <- "Jon Ossoff, GA"

fb.senate <- rbind(fb.senate, ossoff.tibble)

## 14. Doug Collins, GA ========================================================

collins <- as.vector(senatedat[14, "id"])
collins.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = collins,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
collins.response <- adlib_get(params = collins.query, token = token)
collins.tibble <- as_tibble(collins.response, type = "ad")
collins.tibble$candidate <- "Doug Collins, GA"

fb.senate <- rbind(fb.senate, collins.tibble)

## 15. Derrick Grayson, GA =====================================================

grayson <- as.vector(senatedat[15, "id"])
grayson.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = grayson,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
grayson.response <- adlib_get(params = grayson.query, token = token)
grayson.tibble <- as_tibble(grayson.response, type = "ad")
grayson.tibble$candidate <- "Derrick Grayson, GA"

fb.senate <- rbind(fb.senate, grayson.tibble)

## 16. Jamesia James, GA =======================================================

james <- as.vector(senatedat[16, "id"])
james.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = james,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
james.response <- adlib_get(params = james.query, token = token)
james.tibble <- as_tibble(james.response, type = "ad")
james.tibble$candidate <- "Jamesia James, GA"

# I'm getting an error here...james.response is a list of 0, as opposed to a
# list of 14... it would appear that Jamesia James did not have any ads, in
# spite of the fact that they had a page

# fb.senate <- rbind(fb.senate, james.tibble)

## 17. Matt Lieberman, GA ======================================================

lieberman <- as.vector(senatedat[17, "id"])
lieberman.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = lieberman,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
lieberman.response <- adlib_get(params = lieberman.query, token = token)
lieberman.tibble <- as_tibble(lieberman.response, type = "ad")
lieberman.tibble$candidate <- "Matt Lieberman, GA"

fb.senate <- rbind(fb.senate, lieberman.tibble)

## 18. Kelly Loeffler, GA ======================================================

loeffler <- as.vector(senatedat[18, "id"])
loeffler.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = loeffler,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
loeffler.response <- adlib_get(params = loeffler.query, token = token)
loeffler.tibble <- as_tibble(loeffler.response, type = "ad")
loeffler.tibble$candidate <- "Kelly Loeffler, GA"

fb.senate <- rbind(fb.senate, loeffler.tibble)

## 19. Raphael Warnock, GA =====================================================

warnock <- as.vector(senatedat[19, "id"])
warnock.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = warnock,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
warnock.response <- adlib_get(params = warnock.query, token = token)
warnock.tibble <- as_tibble(warnock.response, type = "ad")
warnock.tibble$candidate <- "Raphael Warnock, GA"

fb.senate <- rbind(fb.senate, warnock.tibble)

## 20. Paulette Jordan, ID =====================================================

jordan <- as.vector(senatedat[20, "id"])
jordan.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = jordan,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
jordan.response <- adlib_get(params = jordan.query, token = token)
jordan.tibble <- as_tibble(jordan.response, type = "ad")
jordan.tibble$candidate <- "Paulette Jordan, ID"

fb.senate <- rbind(fb.senate, jordan.tibble)

## 21. Jim Risch, ID =====================================================

risch <- as.vector(senatedat[21, "id"])
risch.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = risch,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
risch.response <- adlib_get(params = risch.query, token = token)
risch.tibble <- as_tibble(risch.response, type = "ad")
risch.tibble$candidate <- "Jim Risch, ID"

fb.senate <- rbind(fb.senate, risch.tibble)

## 22. Dick Durbin, IL =========================================================

durbin <- as.vector(senatedat[22, "id"])
durbin.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = durbin,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
durbin.response <- adlib_get(params = durbin.query, token = token)
durbin.tibble <- as_tibble(durbin.response, type = "ad")
durbin.tibble$candidate <- "Dick Durbin, IL"

fb.senate <- rbind(fb.senate, durbin.tibble)

## 23. Mark Curran, IL =========================================================

curran <- as.vector(senatedat[23, "id"])
curran.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = curran,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
curran.response <- adlib_get(params = curran.query, token = token)
curran.tibble <- as_tibble(curran.response, type = "ad")
curran.tibble$candidate <- "Mark Curran, IL"

fb.senate <- rbind(fb.senate, curran.tibble)

## 24. Joni Ernst, IA ==========================================================

ernst <- as.vector(senatedat[24, "id"])
ernst.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = ernst,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
ernst.response <- adlib_get(params = ernst.query, token = token)
ernst.tibble <- as_tibble(ernst.response, type = "ad")
ernst.tibble$candidate <- "Joni Ernst, IA"

fb.senate <- rbind(fb.senate, ernst.tibble)

## 25 Theresa Greenfield, IA ===================================================

greenfield <- as.vector(senatedat[25, "id"])
greenfield.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = greenfield,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
greenfield.response <- adlib_get(params = greenfield.query, token = token)
greenfield.tibble <- as_tibble(greenfield.response, type = "ad")
greenfield.tibble$candidate <- "Theresa Greenfield, IA"

fb.senate <- rbind(fb.senate, greenfield.tibble)

## 26. Roger Marshall, KS ======================================================

marshall <- as.vector(senatedat[26, "id"])
marshall.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = marshall,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
marshall.response <- adlib_get(params = marshall.query, token = token)
marshall.tibble <- as_tibble(marshall.response, type = "ad")
marshall.tibble$candidate <- "Roger Marshall, KS"

fb.senate <- rbind(fb.senate, marshall.tibble)

## 27. Barbara Bollier, KS =====================================================

bollier <- as.vector(senatedat[27, "id"])
bollier.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = bollier,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
bollier.response <- adlib_get(params = bollier.query, token = token)
bollier.tibble <- as_tibble(bollier.response, type = "ad")
bollier.tibble$candidate <- "Barbara Bollier, KS"

fb.senate <- rbind(fb.senate, bollier.tibble)

## 28. Mitch McConnell, KY =====================================================

mcconnell <- as.vector(senatedat[28, "id"])
mcconnell.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = mcconnell,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
mcconnell.response <- adlib_get(params = mcconnell.query, token = token)
mcconnell.tibble <- as_tibble(mcconnell.response, type = "ad")
mcconnell.tibble$candidate <- "Mitch McConnell, KY"

fb.senate <- rbind(fb.senate, mcconnell.tibble)

## 29. Amy McGrath, KY =========================================================

mcgrath <- as.vector(senatedat[29, "id"])
mcgrath.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = mcgrath,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
mcgrath.response <- adlib_get(params = mcgrath.query, token = token)
mcgrath.tibble <- as_tibble(mcgrath.response, type = "ad")
mcgrath.tibble$candidate <- "Amy McGrath, KY"

fb.senate <- rbind(fb.senate, mcgrath.tibble)

## 30. Bill Cassidy, LA ========================================================

cassidy <- as.vector(senatedat[30, "id"])
cassidy.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = cassidy,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
cassidy.response <- adlib_get(params = cassidy.query, token = token)
cassidy.tibble <- as_tibble(cassidy.response, type = "ad")
cassidy.tibble$candidate <- "Bill Cassidy, LA"

fb.senate <- rbind(fb.senate, cassidy.tibble)

## 31. Champ Edwards, LA =======================================================

edwards <- as.vector(senatedat[31, "id"])
edwards.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = edwards,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
edwards.response <- adlib_get(params = edwards.query, token = token)
edwards.tibble <- as_tibble(edwards.response, type = "ad")
edwards.tibble$candidate <- "Champ Edwards, LA"

# As with Jamesia James, nothing appears to be here

# fb.senate <- rbind(fb.senate, edwards.tibble)

## 32 Adrian Perkins, LA =======================================================

perkins <- as.vector(senatedat[32, "id"])
perkins.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = perkins,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
perkins.response <- adlib_get(params = perkins.query, token = token)
perkins.tibble <- as_tibble(perkins.response, type = "ad")
perkins.tibble$candidate <- "Adrian Perkins, LA"

fb.senate <- rbind(fb.senate, perkins.tibble)

## 33. Antoine Pierce, LA ======================================================

pierce <- as.vector(senatedat[33, "id"])
pierce.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = pierce,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
pierce.response <- adlib_get(params = pierce.query, token = token)
pierce.tibble <- as_tibble(pierce.response, type = "ad")
pierce.tibble$candidate <- "Antoine Pierce, LA"

# Again, as with others, nothing in pierce.response

# fb.senate <- rbind(fb.senate, pierce.tibble)

## 34. Susan Collins, ME =======================================================

collins_ME <- as.vector(senatedat[34, "id"])
collins_ME.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = collins_ME,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
collins_ME.response <- adlib_get(params = collins_ME.query, token = token)
collins_ME.tibble <- as_tibble(collins_ME.response, type = "ad")
collins_ME.tibble$candidate <- "Susan Collins, ME"

fb.senate <- rbind(fb.senate, collins_ME.tibble)

## 35. Sara Gideon, ME =========================================================

gideon <- as.vector(senatedat[35, "id"])
gideon.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = gideon,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
gideon.response <- adlib_get(params = gideon.query, token = token)
gideon.tibble <- as_tibble(gideon.response, type = "ad")
gideon.tibble$candidate <- "Sara Gideon, ME"

# Gideon is > 5000 - will have duplicates

fb.senate <- rbind(fb.senate, gideon.tibble)

gideon2.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = gideon,
  ad_delivery_date_max = "2020-04-01",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
gideon2.response <- adlib_get(params = gideon2.query, token = token)
gideon2.tibble <- as_tibble(gideon2.response, type = "ad")
gideon2.tibble$candidate <- "Sara Gideon, ME"

fb.senate <- rbind(fb.senate, gideon2.tibble)

## 36. Ed Markey, MA ===========================================================

markey <- as.vector(senatedat[36, "id"])
markey.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = markey,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
markey.response <- adlib_get(params = markey.query, token = token)
markey.tibble <- as_tibble(markey.response, type = "ad")
markey.tibble$candidate <- "Ed Markey, MA"

fb.senate <- rbind(fb.senate, markey.tibble)

## 37. Gary Peters, MI =========================================================

peters <- as.vector(senatedat[37, "id"])
peters.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = peters,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
peters.response <- adlib_get(params = peters.query, token = token)
peters.tibble <- as_tibble(peters.response, type = "ad")
peters.tibble$candidate <- "Gary Peters, MI"

fb.senate <- rbind(fb.senate, peters.tibble)

## 38. John James, MI ==========================================================

james_MI <- as.vector(senatedat[38, "id"])
james_MI.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = james_MI,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
james_MI.response <- adlib_get(params = james_MI.query, token = token)
james_MI.tibble <- as_tibble(james_MI.response, type = "ad")
james_MI.tibble$candidate <- "John James, MI"

fb.senate <- rbind(fb.senate, james_MI.tibble)

## 39. Jason Lewis, MN =========================================================

lewis <- as.vector(senatedat[39, "id"])
lewis.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = lewis,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
lewis.response <- adlib_get(params = lewis.query, token = token)
lewis.tibble <- as_tibble(lewis.response, type = "ad")
lewis.tibble$candidate <- "Jason Lewis, MN"

fb.senate <- rbind(fb.senate, lewis.tibble)

## 40. Tina Smith, MN ==========================================================

smith <- as.vector(senatedat[40, "id"])
smith.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = smith,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
smith.response <- adlib_get(params = smith.query, token = token)
smith.tibble <- as_tibble(smith.response, type = "ad")
smith.tibble$candidate <- "Tina Smith, MN"

fb.senate <- rbind(fb.senate, smith.tibble)

### Have not done the ones below here - just setting up ========================

## 41. Mike Espy, MS ===========================================================

espy <- as.vector(senatedat[41, "id"])
espy.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = espy,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
espy.response <- adlib_get(params = espy.query, token = token)
espy.tibble <- as_tibble(espy.response, type = "ad")
espy.tibble$candidate <- "Mike Espy, MS"

fb.senate <- rbind(fb.senate, espy.tibble)

## 42. Cindy Hyde-Smith, MS ====================================================

hydesmith <- as.vector(senatedat[42, "id"])
hydesmith.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = hydesmith,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
hydesmith.response <- adlib_get(params = hydesmith.query, token = token)
hydesmith.tibble <- as_tibble(hydesmith.response, type = "ad")
hydesmith.tibble$candidate <- "Cindy Hyde-Smith, MS"

fb.senate <- rbind(fb.senate, hydesmith.tibble)

## 43. Steve Bullock, MT =======================================================

bullock <- as.vector(senatedat[43, "id"])
bullock.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = bullock,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
bullock.response <- adlib_get(params = bullock.query, token = token)
bullock.tibble <- as_tibble(bullock.response, type = "ad")
bullock.tibble$candidate <- "Steve Bullock, MT"

fb.senate <- rbind(fb.senate, bullock.tibble)

# Bullock is > 5000 - will have duplicates

bullock.query.2 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = bullock,
  ad_delivery_date_max = "2020-06-30",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
bullock.response.2 <- adlib_get(params = bullock.query.2, token = token)
bullock.tibble.2 <- as_tibble(bullock.response.2, type = "ad")
bullock.tibble.2$candidate <- "Steve Bullock, MT"

fb.senate <- rbind(fb.senate, bullock.tibble.2)

## 44. Steve Daines, MT ========================================================

daines <- as.vector(senatedat[44, "id"])
daines.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = daines,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
daines.response <- adlib_get(params = daines.query, token = token)
daines.tibble <- as_tibble(daines.response, type = "ad")
daines.tibble$candidate <- "Steve Daines, MT"

fb.senate <- rbind(fb.senate, daines.tibble)

## 45. Ben Sasse, NE ===========================================================

sasse <- as.vector(senatedat[45, "id"])
sasse.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = sasse,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
sasse.response <- adlib_get(params = sasse.query, token = token)
sasse.tibble <- as_tibble(sasse.response, type = "ad")
sasse.tibble$candidate <- "Ben Sasse, NE"

fb.senate <- rbind(fb.senate, sasse.tibble)

## 46. Chris Janicek, NE ========================================================

janeck <- as.vector(senatedat[46, "id"])
janeck.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = janeck,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
janeck.response <- adlib_get(params = janeck.query, token = token)
janeck.tibble <- as_tibble(janeck.response, type = "ad")
janeck.tibble$candidate <- "Chris Janicek, NE"

fb.senate <- rbind(fb.senate, janeck.tibble)

## 47. Corky Messner, NH =======================================================

messner <- as.vector(senatedat[47, "id"])
messner.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = messner,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
messner.response <- adlib_get(params = messner.query, token = token)
messner.tibble <- as_tibble(messner.response, type = "ad")
messner.tibble$candidate <- "Corky Messner, NH"

fb.senate <- rbind(fb.senate, messner.tibble)

## 48. Jeanne Shaheen, NH ======================================================

shaheen <- as.vector(senatedat[48, "id"])
shaheen.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = shaheen,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
shaheen.response <- adlib_get(params = shaheen.query, token = token)
shaheen.tibble <- as_tibble(shaheen.response, type = "ad")
shaheen.tibble$candidate <- "Jeanne Shaheen, NH"

fb.senate <- rbind(fb.senate, shaheen.tibble)

## 49. Cory Booker, NJ =========================================================

booker <- as.vector(senatedat[49, "id"])
booker.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = booker,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
booker.response <- adlib_get(params = booker.query, token = token)
booker.tibble <- as_tibble(booker.response, type = "ad")
booker.tibble$candidate <- "Cory Booker, NJ"

fb.senate <- rbind(fb.senate, booker.tibble)

## 50. Rik Mehta, NJ ===========================================================

mehta <- as.vector(senatedat[50, "id"])
mehta.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = mehta,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
mehta.response <- adlib_get(params = mehta.query, token = token)
mehta.tibble <- as_tibble(mehta.response, type = "ad")
mehta.tibble$candidate <- "Rik Mehta, NJ"

fb.senate <- rbind(fb.senate, mehta.tibble)

## 51. Mark Ronchetti, NM ======================================================

ronchetti <- as.vector(senatedat[51, "id"])
ronchetti.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = ronchetti,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
ronchetti.response <- adlib_get(params = ronchetti.query, token = token)
ronchetti.tibble <- as_tibble(ronchetti.response, type = "ad")
ronchetti.tibble$candidate <- "Mark Ronchetti, NM"

fb.senate <- rbind(fb.senate, ronchetti.tibble)

## 52. Ben Lujan, NM ===========================================================

lujan <- as.vector(senatedat[52, "id"])
lujan.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = lujan,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
lujan.response <- adlib_get(params = lujan.query, token = token)
lujan.tibble <- as_tibble(lujan.response, type = "ad")
lujan.tibble$candidate <- "Ben Lujan, NM"

fb.senate <- rbind(fb.senate, lujan.tibble)

## 53. Thom Tillis, NC =========================================================

tillis <- as.vector(senatedat[53, "id"])
tillis.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = tillis,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
tillis.response <- adlib_get(params = tillis.query, token = token)
tillis.tibble <- as_tibble(tillis.response, type = "ad")
tillis.tibble$candidate <- "Thom Tillis, NC"

fb.senate <- rbind(fb.senate, tillis.tibble)

## 54. Cal Cunningham, NC ======================================================

cunningham <- as.vector(senatedat[54, "id"])
cunningham.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = cunningham,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
cunningham.response <- adlib_get(params = cunningham.query, token = token)
cunningham.tibble <- as_tibble(cunningham.response, type = "ad")
cunningham.tibble$candidate <- "Cal Cunningham, NC"

fb.senate <- rbind(fb.senate, cunningham.tibble)

# Cunningham is > 5,000

cunningham.query.2 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = cunningham,
  ad_delivery_date_max = "2020-07-15",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
cunningham.response.2 <- adlib_get(params = cunningham.query.2, token = token)
cunningham.tibble.2 <- as_tibble(cunningham.response.2, type = "ad")
cunningham.tibble.2$candidate <- "Cal Cunningham, NC"

fb.senate <- rbind(fb.senate, cunningham.tibble.2)

## 55. Jim Inhofe, OK ==========================================================

inhofe <- as.vector(senatedat[55, "id"])
inhofe.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = inhofe,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
inhofe.response <- adlib_get(params = inhofe.query, token = token)
inhofe.tibble <- as_tibble(inhofe.response, type = "ad")
inhofe.tibble$candidate <- "Jim Inhofe, OK"

fb.senate <- rbind(fb.senate, inhofe.tibble)

## 56. Abby Broyles, OK ==========================================================

broyles <- as.vector(senatedat[56, "id"])
broyles.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = broyles,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
broyles.response <- adlib_get(params = broyles.query, token = token)
broyles.tibble <- as_tibble(broyles.response, type = "ad")
broyles.tibble$candidate <- "Abby Broyles, OK"

fb.senate <- rbind(fb.senate, broyles.tibble)

## 57. Jo Rae Perkins, OR ======================================================

raeperkins <- as.vector(senatedat[57, "id"])
raeperkins.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = raeperkins,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
raeperkins.response <- adlib_get(params = raeperkins.query, token = token)
raeperkins.tibble <- as_tibble(raeperkins.response, type = "ad")
raeperkins.tibble$candidate <- "Jo Rae Perkins, OR"

# As with others, Jo Rae Perkins has none

# fb.senate <- rbind(fb.senate, raeperkins.tibble)

## 58. Jeff Merkley, OR ========================================================

merkley <- as.vector(senatedat[58, "id"])
merkley.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = merkley,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
merkley.response <- adlib_get(params = merkley.query, token = token)
merkley.tibble <- as_tibble(merkley.response, type = "ad")
merkley.tibble$candidate <- "Jeff Merkley, OR"

fb.senate <- rbind(fb.senate, merkley.tibble)

## 59. John Reed, RI ===========================================================

reed <- as.vector(senatedat[59, "id"])
reed.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = reed,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
reed.response <- adlib_get(params = reed.query, token = token)
reed.tibble <- as_tibble(reed.response, type = "ad")
reed.tibble$candidate <- "John Reed, RI"

fb.senate <- rbind(fb.senate, reed.tibble)

## 60. Allen Waters, RI ========================================================

waters <- as.vector(senatedat[60, "id"])
waters.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = waters,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
waters.response <- adlib_get(params = waters.query, token = token)
waters.tibble <- as_tibble(waters.response, type = "ad")
waters.tibble$candidate <- "Allen Waters, RI"

fb.senate <- rbind(fb.senate, waters.tibble)

## 61. Lindsey Graham, SC =======================================================

graham <- as.vector(senatedat[61, "id"])
graham.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = graham,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
graham.response <- adlib_get(params = graham.query, token = token)
graham.tibble <- as_tibble(graham.response, type = "ad")
graham.tibble$candidate <- "Lindsey Graham, SC"

fb.senate <- rbind(fb.senate, graham.tibble)

# Graham has > 5000

graham.query.2 <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = graham,
  ad_delivery_date_max = "2020-05-10",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
graham.response.2 <- adlib_get(params = graham.query.2, token = token)
graham.tibble.2 <- as_tibble(graham.response.2, type = "ad")
graham.tibble.2$candidate <- "Lindsey Graham, SC"

fb.senate <- rbind(fb.senate, graham.tibble.2)

## 62. Jaime Harrison, SC =======================================================

harrison <- as.vector(senatedat[62, "id"])
harrison.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = harrison,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
harrison.response <- adlib_get(params = harrison.query, token = token)
harrison.tibble <- as_tibble(harrison.response, type = "ad")
harrison.tibble$candidate <- "Jaime Harrison, SC"

fb.senate <- rbind(fb.senate, harrison.tibble)

## 63. Mike Rounds, SD =======================================================

rounds <- as.vector(senatedat[63, "id"])
rounds.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = rounds,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
rounds.response <- adlib_get(params = rounds.query, token = token)
rounds.tibble <- as_tibble(rounds.response, type = "ad")
rounds.tibble$candidate <- "Mike Rounds, SD"

fb.senate <- rbind(fb.senate, rounds.tibble)

## 64. Dan Ahlers, SD =======================================================

ahlers <- as.vector(senatedat[64, "id"])
ahlers.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = ahlers,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
ahlers.response <- adlib_get(params = ahlers.query, token = token)
ahlers.tibble <- as_tibble(ahlers.response, type = "ad")
ahlers.tibble$candidate <- "Dan Ahlers, SD"

fb.senate <- rbind(fb.senate, ahlers.tibble)

## 65. Bill Hagerty, TN ========================================================

hagerty <- as.vector(senatedat[65, "id"])
hagerty.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = hagerty,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
hagerty.response <- adlib_get(params = hagerty.query, token = token)
hagerty.tibble <- as_tibble(hagerty.response, type = "ad")
hagerty.tibble$candidate <- "Bill Hagerty, TN"

fb.senate <- rbind(fb.senate, hagerty.tibble)

## 66. Marquita Bradshaw, TN ===================================================

bradshaw <- as.vector(senatedat[66, "id"])
bradshaw.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = bradshaw,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
bradshaw.response <- adlib_get(params = bradshaw.query, token = token)
bradshaw.tibble <- as_tibble(bradshaw.response, type = "ad")
bradshaw.tibble$candidate <- "Marquita Bradshaw, TN"

fb.senate <- rbind(fb.senate, bradshaw.tibble)

## 67. John Cornyn, TX =========================================================

cornyn <- as.vector(senatedat[67, "id"])
cornyn.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = cornyn,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
cornyn.response <- adlib_get(params = cornyn.query, token = token)
cornyn.tibble <- as_tibble(cornyn.response, type = "ad")
cornyn.tibble$candidate <- "John Cornyn, TX"

fb.senate <- rbind(fb.senate, cornyn.tibble)

## 68. Mary Hegar, TX ==========================================================

hegar <- as.vector(senatedat[68, "id"])
hegar.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = hegar,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
hegar.response <- adlib_get(params = hegar.query, token = token)
hegar.tibble <- as_tibble(hegar.response, type = "ad")
hegar.tibble$candidate <- "Mary Hegar, TX"

fb.senate <- rbind(fb.senate, hegar.tibble)

## 69. Mark Warner, VA =========================================================

warner <- as.vector(senatedat[69, "id"])
warner.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = warner,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
warner.response <- adlib_get(params = warner.query, token = token)
warner.tibble <- as_tibble(warner.response, type = "ad")
warner.tibble$candidate <- "Mark Warner, VA"

fb.senate <- rbind(fb.senate, warner.tibble)

## 70. Daniel Gade, VA =========================================================

gade <- as.vector(senatedat[70, "id"])
gade.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = gade,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
gade.response <- adlib_get(params = gade.query, token = token)
gade.tibble <- as_tibble(gade.response, type = "ad")
gade.tibble$candidate <- "Daniel Gade, VA"

fb.senate <- rbind(fb.senate, gade.tibble)

## 71. Shelley Moore Capito, WV ================================================

moorecapito <- as.vector(senatedat[71, "id"])
moorecapito.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = moorecapito,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
moorecapito.response <- adlib_get(params = moorecapito.query, token = token)
moorecapito.tibble <- as_tibble(moorecapito.response, type = "ad")
moorecapito.tibble$candidate <- "Shelley Moore Capito, WV"

# No ads from Shelley Moore Capito
# fb.senate <- rbind(fb.senate, moorecapito.tibble)

## 72. Paula Jean Swearengin, WV ===============================================

swearengin <- as.vector(senatedat[72, "id"])
swearengin.query <- adlib_build_query(
  ad_reached_countries = "US",
  ad_active_status = "ALL",
  search_page_ids = swearengin,
  ad_delivery_date_max = "2020-11-03",
  ad_delivery_date_min = "2020-01-01",
  limit = 10000,
  fields = "ad_data"
)
swearengin.response <- adlib_get(params = swearengin.query, token = token)
swearengin.tibble <- as_tibble(swearengin.response, type = "ad")
swearengin.tibble$candidate <- "Paula Jean Swearengin, WV"

fb.senate <- rbind(fb.senate, swearengin.tibble)

# Saving data ==========================================================
# library(dplyr)

fb.senate.distinct <- distinct(fb.senate)

save(fb.senate.distinct, file = "data/raw/fb/fb-senate-2020-ads.rda")
