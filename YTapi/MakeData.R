# Apple Vision Pro
# 1 - dtp6b76pMak - Using Apple Vision Pro: What It’s Actually Like! 30k comments
# 2 - UvkgmyfMPks - the thing no one will say about Apple Vision Pro Casey Neistat 13k comments
# 3 - 8xI10SFgzQ8 - I Spent 24 Hours Wearing Apple’s Vision Pro Headset | WSJ 4k comments
# 4 - SSC0RkJuBVw - Apple Vision Pro - Is it worth $3500? - 18k comments
# 5 - SqB0lUcqFbA - Hating Apple is Getting REALLY Hard - WWDC 2023 - 10k comments
# 6 - hdwaWxY11jQ - Apple Vision Pro review: magic, until it’s not - 5k comments
# 7 - CaWt6-xe29k - Apple Vision Pro - Unboxing, Review and demos! - 2k comments
# 8 - M2LWcHIyNWQ - How Apple's Vision Pro Will Change Society Forever - 15k comments
# 9 - n7hJlyVDEc8 - The Real Reason To Care About The Apple Vision Pro - 2k comments
# 10 - LmcWMjBpYBU - Be gentle with the Apple Vision Pro - ITS PLASTIC!! - 10k comments

library(tuber)
yt_oauth(app_id = "879991409679-m2uk2q32s0akujjmbk8pm22c9bdb9dmc.apps.googleusercontent.com",
          app_secret = "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", token = "")

MarquesBrownlee <- get_all_comments('dtp6b76pMak')
CaseyNeistat <- get_all_comments('UvkgmyfMPks')
WallStreetJournal <- get_all_comments('8xI10SFgzQ8')
MrWhoseTheBoss <- get_all_comments('SSC0RkJuBVw')
LinusTechTips <- get_all_comments('SqB0lUcqFbA')
TheVerge <- get_all_comments('hdwaWxY11jQ')
iJustine <- get_all_comments('CaWt6-xe29k')
Moon <- get_all_comments('M2LWcHIyNWQ')
CleoAbram <- get_all_comments('n7hJlyVDEc8')
JerryRigEverything <- get_all_comments('LmcWMjBpYBU')

MarquesBrownlee <- cbind('MarquesBrownlee', MarquesBrownlee$textOriginal, MarquesBrownlee$likeCount)
colnames(MarquesBrownlee) <- c('Channel', 'Comment', 'likeCount')

CaseyNeistat <- cbind('CaseyNeistat', CaseyNeistat$textOriginal, CaseyNeistat$likeCount)
colnames(CaseyNeistat) <- c('Channel', 'Comment', 'likeCount')

WallStreetJournal <- cbind('WallStreetJournal', WallStreetJournal$textOriginal, WallStreetJournal$likeCount)
colnames(WallStreetJournal) <- c('Channel', 'Comment', 'likeCount')

MrWhoseTheBoss <- cbind('MrWhoseTheBoss', MrWhoseTheBoss$textOriginal, MrWhoseTheBoss$likeCount)
colnames(MrWhoseTheBoss) <- c('Channel', 'Comment', 'likeCount')

LinusTechTips <- cbind('LinusTechTips', LinusTechTips$textOriginal, LinusTechTips$likeCount)
colnames(LinusTechTips) <- c('Channel', 'Comment', 'likeCount')

TheVerge <- cbind('TheVerge', TheVerge$textOriginal, TheVerge$likeCount)
colnames(TheVerge) <- c('Channel', 'Comment', 'likeCount')

iJustine <- cbind('iJustine', iJustine$textOriginal, iJustine$likeCount)
colnames(iJustine) <- c('Channel', 'Comment', 'likeCount')

Moon <- cbind('Moon', Moon$textOriginal, Moon$likeCount)
colnames(Moon) <- c('Channel', 'Comment', 'likeCount')

CleoAbram <- cbind('CleoAbram', CleoAbram$textOriginal, CleoAbram$likeCount)
colnames(CleoAbram) <- c('Channel', 'Comment', 'likeCount')

JerryRigEverything <- cbind('JerryRigEverything', JerryRigEverything$textOriginal, JerryRigEverything$likeCount)
colnames(JerryRigEverything) <- c('Channel', 'Comment', 'likeCount')

comments1March = as.data.frame(rbind(CaseyNeistat, CleoAbram, iJustine, JerryRigEverything,
                              LinusTechTips, MarquesBrownlee, Moon, MrWhoseTheBoss,
                              TheVerge, WallStreetJournal))

comments1March$likeCount = as.numeric(comments1March$likeCount)
write.csv(comments1March, 'comments1March.csv')
