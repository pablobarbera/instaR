# app_id <- "AAAAAA"
# app_secret <- "YYYYYXXXXYYY"
# token <- instaOAuth(app_id, app_secret)

load("~/insta_token")
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

# downloading my 100 most recent pictures
pablo <- getUser("pablobarbera", token, n=300, folder="pablo_pics")

# downloading 100 recent pictures from someone else
wh <- getUser("whitehouse", token, n=100, folder="WH_pics")

# get list of followers of a user
mccain <- getFollowers("senjohnmccain", token)

# get list of users a given user follows
mccain2 <- getFollows("senjohnmccain", token)

# TO-DO list:
# get feed of pictures from friends
# add min_id, max_id as arguments
# pictures from location IDs
# download list of likes/comments for given pictures
# download popular pictures
# get information about a hashtag (count)
# search for tags by name

