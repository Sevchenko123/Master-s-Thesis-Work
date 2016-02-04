library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(igraph)
library(googleVis)

mydb=dbConnect(MySQL(),user='derek',password='derek1',dbname='7cot-fall15',host='130.108.85.104')

a1 = dbSendQuery(mydb,'select * from normal_users')
non_harassers = fetch(a1,n=-1)

d3 = dbSendQuery(mydb,'select t.signupDateU, t.lastLoginU, t.signupDistressLevel, t.forumUpvotes, t.compassionHearts, t.coins, t.groupSupportMsgs, t.growthPoints, f.bannerID from usersMembers as t, z_h as f where  f.bannerID = t.memID')
d4 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "convRequestGeneral" and f.bannerID = t.actUserID')
d5 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "acctLogin" and f.bannerID = t.actUserID')
d6 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "forumPost" and f.bannerID = t.actUserID') 
d7 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "forumView" and f.bannerID = t.actUserID')
d8 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "pageviewApp" and f.bannerID = t.actUserID') 
d9 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "pageviewWeb" and f.bannerID = t.actUserID')
d10 = dbSendQuery(mydb,'select t.actCount from actionLogging as t, z_h as f where action = "helpGuideView" and f.bannerID = t.actUserID')

d11 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "convRequestGeneral" and f.memID=t.actUserID')
d12 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "acctLogin" and f.memID=t.actUserID')
d13 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "forumPost" and f.memID=t.actUserID')
d14 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "forumView" and f.memID=t.actUserID')
d15 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "pageviewApp" and f.memID=t.actUserID')
d16 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "pageviewWeb" and f.memID=t.actUserID')
d17 = dbSendQuery(mydb,'select t.actCount from actionLogging as t,normal_users as f where action = "helpGuideView" and f.memID=t.actUserID')

n1 = fetch(d11,n=-1) # conv request general (normal Users)
n2 = fetch(d12,n=-1) # Account Logins 
n3 = fetch(d13,n=-1) # Forum Posts
n4 = fetch(d14,n=-1) # Forum Views
n5 = fetch(d15,n=-1) # Page Views
n6 = fetch(d16,n=-1) # Page Views Web
n7 = fetch(d17,n=-1) # Help Guide Views


h4 = fetch(d4,n=-1) # conv request general (harassers)
h5 = fetch(d5,n=-1) # Account Logins
h6 = fetch(d6,n=-1) # Forum Post
h7 = fetch(d7,n=-1) # Forum View
h8 = fetch(d8,n=-1) # Page View App
h9 = fetch(d9,n=-1) # Page View Web
h10 = fetch(d10,n=-1) # Help Guide View

h4 = h4[complete.cases(h4),]
h5 = h5[complete.cases(h5),]
h6 = h6[complete.cases(h6),]
h7 = h7[complete.cases(h7),]
h8 = h8[complete.cases(h8),]
h9 = h9[complete.cases(h9),]
h10 = h10[complete.cases(h10),]



harasser = fetch(d3,n=-1)
harasser = harasser[complete.cases(harasser),]

dl = data.frame(harasser$signupDistressLevel) # distress levels
fuv = data.frame(harasser$forumUpvotes)       # forum up votes
ch = data.frame(harasser$compassionHearts)    # compassion hearts
c = data.frame(harasser$coins)                # coins
gsm = data.frame(harasser$groupSupportMsgs)   # group support messages
gp = data.frame(harasser$growthPoints)        # growth points
ll = data.frame(harasser$lastLoginU)          # Last Login time
su = data.frame(harasser$signupDateU)         # Sign up date


DL = gvisHistogram(dl, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Distress Levels in harassers',
                                        width = 500,hAxis = "{showTextEvery: 5,title:
         'Disress Levels'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

FUV = gvisHistogram(fuv, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Forum Up Votes in harassers',
                                         width = 500,hAxis = "{showTextEvery: 5,title:
         'Forum Up Votes'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

CH = gvisHistogram(ch, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Compassion Hearts in harassers',
                                       width = 500,hAxis = "{showTextEvery: 5,title:
         'Compassion Hearts'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

C = gvisHistogram(c, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Coins in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Coins'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

GSM = gvisHistogram(gsm, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Group Support msgs in harassers',
                                        width = 500,hAxis = "{showTextEvery: 5,title:
         'Group Support Messages'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

GP = gvisHistogram(gp, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Growth Points in harassers',
                                       width = 500,hAxis = "{showTextEvery: 5,title:
         'Growth Points'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

LL = gvisHistogram(ll, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Last Login TImes in harassers',
                                           width = 500,hAxis = "{showTextEvery: 5,title:
         'Last Login Times'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

SU = gvisHistogram(su, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Sign Up Dates in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Sign Up Dates U'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))


H4 = gvisHistogram(h4, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Conversation Requests in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Conversation Requests'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H5 = gvisHistogram(h5, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Account Logins in harassers',
                                           width = 500,hAxis = "{showTextEvery: 5,title:
         'Account Logins'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H6 = gvisHistogram(h6, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Forum Posts in harassers',
                                           width = 500,hAxis = "{showTextEvery: 5,title:
         'Forum Posts'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H7 = gvisHistogram(h7, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Forum Views in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Forum Views'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H8 = gvisHistogram(h8, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Page Views App in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Page Views App'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H9 = gvisHistogram(h9, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Page Views Web in harassers',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Page View Web'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

H10 = gvisHistogram(h10, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Help Guide Views in harassers',
                                            width = 500,hAxis = "{showTextEvery: 5,title:
         'Help Guide Views'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))



mrg = gvisMerge(DL,FUV)
mrg1 = gvisMerge(GP,GSM)
mrg2 = gvisMerge(CH,C)
mrg3 = gvisMerge(LL,SU)
mrg4 = gvisMerge(H4,H5)
mrg5 = gvisMerge(H6,H7)
mrg6 = gvisMerge(H8,H9)


# Normal Users 
n_dl = data.frame(non_harassers$signupDistressLevel) # distress levels
n_fuv = data.frame(non_harassers$forumUpVotes)       # forum up votes
n_ch = data.frame(non_harassers$compassionHearts)    # compassion hearts
n_c = data.frame(non_harassers$coins)                # coins
n_gsm = data.frame(non_harassers$groupSupportMsgs)   # group support messages
n_gp = data.frame(non_harassers$growthPoints)        # growth points
n_ll = data.frame(non_harassers$lastLoginU)          # Last Login time
n_su = data.frame(non_harassers$signupDateU)         # Sign up date

N_DL = gvisHistogram(n_dl, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Distress Levels in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Disress Levels'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

N_FUV = gvisHistogram(n_fuv, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Forum Up Votes in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Disress Levels'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

N_CH = gvisHistogram(n_ch, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Compassion Hearts in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Compassion Hearts'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

N_C = gvisHistogram(n_c, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Coins in harassers',
                                    width = 500,hAxis = "{showTextEvery: 5,title:
                                    'Coins'}",vAxis = "{gridlines : {count:4}, title :
                                    'Frequency'}"))

N_GSM = gvisHistogram(n_gsm, options = list(histogram = "{bucketSize
                                        :1}",legend = "none",title ='Distribution of Group Support msgs in Normal Users',
                                        width = 500,hAxis = "{showTextEvery: 5,title:
                                        'Group Support Messages'}",vAxis = "{gridlines : {count:4}, title :
                                        'Frequency'}"))

N_GP = gvisHistogram(n_gp, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Growth Points in Normal Users',
                                       width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Growth Points'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N_LL = gvisHistogram(n_ll, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Last Login Times in Normal Users',
                                           width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Last Login Times'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N_SU = gvisHistogram(n_su, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Sign Up Dates in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Sign Up Dates U'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))


N1 = gvisHistogram(n1, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Conversation Requests in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
         'Conversation Requests'}",vAxis = "{gridlines : {count:4}, title :
           'Frequency'}"))

N2 = gvisHistogram(n2, options = list(histogram = "{bucketSize
     :1}",legend = "none",title ='Distribution of Account Logins in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Account Logins'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N3 = gvisHistogram(n3, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Forum Posts in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Forum Posts'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N4 = gvisHistogram(n4, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Forum Views in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Forum Views'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N5 = gvisHistogram(n5, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Page Views App in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Page Views App'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N6 = gvisHistogram(n6, options = list(histogram = "{bucketSize
                                      :1}",legend = "none",title ='Distribution of Page Views Web in Normal Users',
                                      width = 500,hAxis = "{showTextEvery: 5,title:
                                      'Page View Web'}",vAxis = "{gridlines : {count:4}, title :
                                      'Frequency'}"))

N7 = gvisHistogram(n7, options = list(histogram = "{bucketSize
                                        :1}",legend = "none",title ='Distribution of Help Guide Views in Normal Users',
                                            width = 500,hAxis = "{showTextEvery: 5,title:
                                        'Help Guide Views'}",vAxis = "{gridlines : {count:4}, title :
                                        'Frequency'}"))






n_mrg = gvisMerge(N_DL,N_FUV)
n_mrg1 = gvisMerge(N_GP,N_GSM)
n_mrg2 = gvisMerge(N_CH,N_C)
n_mrg3 = gvisMerge(N_LL,N_SU)

n_mrg4 = gvisMerge(N1,N2)
n_mrg5 = gvisMerge(N3,N4)
n_mrg6 = gvisMerge(N5,N6)

plot(mrg) # Harassers
plot(mrg1)
plot(mrg2)
plot(mrg3)
plot(mrg4)
plot(mrg5)
plot(mrg6)
plot(H10)

plot(n_mrg) # Normal Users 
plot(n_mrg1)
plot(n_mrg2)
plot(n_mrg3)
plot(n_mrg4)
plot(n_mrg5)
plot(n_mrg6)
plot(N7)
