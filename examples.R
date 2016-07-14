# IMPORTANT NOTE: most of the examples below assume you have a token
# with the permissions required to access public content.
# Without this token, only getTagCount, getUserMedia, and getUser are
# available to any user, and with some limitations (see the documentation
# for each function for more details).

# app_id <- "AAAAAA"
# app_secret <- "YYYYYXXXXYYY"
# token <- instaOAuth(app_id, app_secret)

load("~/Dropbox/credentials/instagram/insta_token")
library(instaR)

# getting pictures with #euromaidan hashtag
euromaidan <- searchInstagram("euromaidan", token, n=100, folder="euromaidan")

# getting pictures with #obama hashtag
obama <- searchInstagram("obama", token, n=100, folder="obama")

# search for pictures near specific locations
tsq <- searchInstagram(lat=40.7577, lng=-73.9857, distance=500, 
    token=token, n=500, folder="timessquare")

maidan <- searchInstagram(lat=50.45, lng=30.524, distance=1000, 
    token=token, n=500, folder="maidan")

north <- searchInstagram(lat=90, lng=0, distance=5000, 
    token=token, n=500, folder="northpole")

gezi <- searchInstagram(lat=41.0383, lng=28.9869, distance=500, 
    token=token, n=200, folder="gezi")

# search around location around a specific time (default time span is 5 days, max is 7)
# maxdate and mindate use the input YYYY-MM-DD HH:MM:SS GMT
# e.g. mindate="2015-01-10 20:00:00 EST"
tsq <- searchInstagram(lat=40.7577, lng=-73.9857, distance=500, 
    token=token, n=500, folder="timessquare", mindate="2015-01-01", maxdate="2015-01-03")

# downloading my 100 most recent pictures
pablo <- getUserMedia("pablobarbera", token, n=100, folder="pablo_pics")

# downloading 100 recent pictures from someone else
wh <- getUserMedia("whitehouse", token, n=100, folder="WH_pics")

# get list of followers of a user
mccain <- getFollowers("senjohnmccain", token)

# get list of users a given user follows
mccain2 <- getFollows("senjohnmccain", token)

# get list of comments for an instragram picture (using the first of the wh pictures downloaded above)
wh_comments <- getComments(wh$id[1], token)

# get list of likes for an instagram picture
wh_likes <- getLikes(wh$id[1], token)

# get tag count for a tag
tag_count <- getTagCount("obama", token)

# get user information
obama_info <- getUser("obama", token)

# get information about a location, using a location ID
location_info <- getLocation(423423, token)

# TO-DO list:
# get feed of pictures from friends
# add min_id, max_id as arguments
# pictures from location IDs
# search for tags by name
