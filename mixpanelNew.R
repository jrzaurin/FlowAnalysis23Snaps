###############################################################
####import data from Mix Panel#################################
###############################################################

dir <- "/Users/javier/Working/23snaps/MixPanel/FlowsAndSequences/20150501-20151201"
setwd(dir)
Events <- c("Flow: Change Book Cover","Flow: Change Book Options"
            ,"Flow: Deeplink","Flow: Enter Recipient Show Contacts"
            ,"Flow: Invite - Babycenter","Flow: Invite - Contact Selected"
            ,"Flow: Invite - Facebook","Flow: Invite - Gmail"
            ,"Flow: Invite - WhatsApp","Flow: Invite - Yahoo"
            ,"Flow: Invite Link Created","Flow: Invite Send"
            ,"Flow: Invite Send Cancel","Flow: Invite Send Complete"
            ,"Flow: Invite Send Failed","Flow: Invite Send Success"
            ,"Flow: My Friends Tapped","Flow: Onboard Slideshow Cancel"
            ,"Flow: Order Complete","Flow: Order Payment Complete"
            ,"Flow: Prompt Tap","Flow: Suggested Friend Action"
            ,"Sheet: Book Preview","Sheet: Collections","Sheet: Customise Order"
            ,"Sheet: Enter Recipient","Sheet: Help"
            ,"Sheet: Invitation Message","Sheet: Invite Someone"
            ,"Sheet: Login","Sheet: Manage My Family"
            ,"Sheet: More","Sheet: My Family"
            ,"Sheet: My Friends","Sheet: News Feed"
            ,"Sheet: Notifications","Sheet: Onboard Slideshow Page 1"
            ,"Sheet: Onboard Slideshow Page 2","Sheet: Order Book"
            ,"Sheet: Order Payment","Sheet: Order Prints"
            ,"Sheet: Order Shipping Address","Sheet: Post", "Sheet: Register"
            ,"Sheet: Select Address Book Contact","Sheet: Settings"
            ,"Sheet: Suggested Friends","Sheet: Time Machine")

Key <- "961cda4cf1bc2d54cf2fee293e57ca4d"
Secret <- "499853cd5fc98b061a675f3448779031"
name.l1 <- "mixac1"

NewEvents <- c("Accept Connect Request","Accept Partner Connect Request"
               ,"Activity Child Request Approved","Activity Child Request Declined"
               ,"Activity Export Requested","Activity SharedAdd Activity"
               ,"Add AnnotationAdd ChildCampaign Sent","Comment LikedEmail Digest View"
               ,"Flow: Change Book Cover","Flow: Change Book Options"
               ,"Flow: Change Shipping Location","Flow: Enter Recipient Show Contacts"
               ,"Flow: Invite - Babycenter","Flow: Invite - Contact Selected"
               ,"Flow: Invite - Facebook","Flow: Invite - Gmail"
               ,"Flow: Invite - WhatsApp","Flow: Invite - Yahoo"
               ,"Flow: Invite Link Created","Flow: Invite Send"
               ,"Flow: Invite Send Cancel","Flow: Invite Send Complete"
               ,"Flow: Invite Send Failed","Flow: Invite Send Success"
               ,"Flow: Logout","Flow: My Friends Tapped"
               ,"Flow: Onboard Slideshow Cancel","Flow: Order Complete"
               ,"Flow: Order Payment Complete","Flow: Suggested Friend Action"
               ,"Notification Bounced","Notification Marked Spam"
               ,"Notification Opened","Notification Sent"
               ,"Order Created","Register"
               ,"Register Partial","Send Connect Request"
               ,"Send Group Invitation","Send Partner Connect Request"
               ,"Sheet: Book Preview","Sheet: CollectionsS","Sheet: Customise Order"
               ,"Sheet: Enter Recipient","Sheet: Help"
               ,"Sheet: Invitation Message","Sheet: Invite Someone"
               ,"Sheet: Login","Sheet: Manage My Family"
               ,"Sheet: More","Sheet: My Family","Sheet: My Friends"
               ,"Sheet: News Feed","Sheet: Notifications"
               ,"Sheet: Onboard Slideshow Page 1","Sheet: Onboard Slideshow Page 2"
               ,"Sheet: Order Book","Sheet: Order Payment"
               ,"Sheet: Order Prints","Sheet: Order Shipping Address"
               ,"Sheet: PostSheet: Register","Sheet: Select Address Book Contact"
               ,"Sheet: Settings","Sheet: Suggested Friends"
               ,"Sheet: Time Machine","User Session Started"
               ,"install")

NewEvents <- NewEvents[-which(NewEvents %in% Events)]

NewKey <- "9a0dbb746d872e82f94ab22bdf8f7081"
NewSecret <- "bbe3613d42e9677a7a2390ba0f9d1c7c"
name.l2 <- "mixac2"

event.list <- list(Events, NewEvents)
Key.list <- list(Key, NewKey)
Secret.list <- list(Secret, NewSecret)
names.list <- list(name.l1,name.l2)

mixpanelNew <- function (key,secret,events,lname) {
    library(RCurl)
    library(rjson)
    library(digest)
    library(httr)

    alleventsList <- list()

    expire <- as.integer(as.numeric(as.POSIXlt(Sys.time()))) + 36000 
    
    for (i in 1:length(events)){
        
        
        event <- events[i]
        event <- paste('["',event,'"]',sep="",collapse=NULL)
        
        from_date <- "2015-01-05"
        to_date <- "2015-01-12"
        
        ## Set the arguments
        args_sig <- paste('event=',event,"expire=",expire,"from_date=",from_date,
                          "to_date=",to_date,sep="",collapse=NULL)
        
        args_url <- paste('event=',URLencode(event),"&expire=",expire,"&from_date=",from_date,
                          "&to_date=",to_date,sep="",collapse=NULL)
        
        ## Create the hashed Signature
        sig <- paste("api_key=",key,args_sig,secret,sep="",collapse=NULL)
        hashed_sig <- digest(sig, algo="md5", serialize = FALSE)
        
        ## Create the URL with the full authorization string
        url <- paste("http://data.mixpanel.com/api/2.0/export/?","api_key=",key,"&",
                     args_url,"&sig=",hashed_sig,sep="",collapse=NULL)
        
        ## Connect to the Mixpanel API and save data
        eventList <- lapply(readLines(url), function(x) fromJSON(x))
        alleventsList <- append(alleventsList, eventList)
    }
    assign(lname,alleventsList,envir = .GlobalEnv)
}


for (i in 1:length(event.list)){
    mixpanelNew(Key.list[[i]],
                Secret.list[[i]],
                event.list[[i]],
                names.list[[i]])
}

save(mixac1, file = "mixac1.RData")
save(mixac2, file = "mixac2.RData")
