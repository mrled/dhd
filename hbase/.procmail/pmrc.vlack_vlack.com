SHELL = /usr/local/bin/bash

MAILDIR=$HOME/.Maildir/ 
DEFAULT=$MAILDIR 
LOGFILE=$HOME/.procmail.log
LOGABSTRACT = "all"
VERBOSE = "on"

PMDIR=$HOME/.procmail
#DOMAIN=howe\.textdrive\.com


#:0 hc
#! varchive@vlack.com
#:0 hc
#! fullvarchive@mrled.org

:0 hc
/home/mledbetter/Backup/Maildir


### Locking methods:
# :0: 				Let procmail figure out how to lock it
# :0 				No locking
# :0:filename.lock 	Explicitly name a lockfile to use

### TextDrive Stuff & Junk
# you should prefix all your mailboxes with ".INBOX", like ".INBOX.lists/"
# you should NOT use Mail.app's prefix to .INBOX ... the prefix should be empty

### A bit on condition scoring...
# (mostly taken from 
# http://perlcode.org/tutorials/procmail/proctut/proctip2.pod )
#
#     :0:
#     * 1^0 ^Subject:.*results
#     * 1^0 ^Subject:.*mlm
#     /var/mail/temp
# 
# ... the above will move the message to /var/mail/temp if either
# condition applies.
# This method checks all conditions, even if the first is met... a 
# more efficient way of doing this is by using the supremium score
# in procmail, which happens to be 2147483647. If this is done, 
# procmail checks each condition until one is reached, and then ceases
# the checking of condidions. 
#
#     :0:
#     * 2147483647^0 ^Subject:.*results
#     * 2147483647^0 ^Subject:.*mlm
#     /var/mail/temp
# 
# One final way to increase simplicity is by setting
#     SPR=9876543210 (SPR can be any number > 2147483647
#     :0:
#     * $ $SPR^0 ^Subject:.*results
#     * $ $SPR^0 ^Subject:.*mlm
#     /var/mail/temp
#
# Note: both dollar signs are required. 

### Random stupid things not to forget:
# 1 close your freaking folders with a "/". it's ".lists.sunhelp-geeks/"
# 2 start with a "*", don't just begin your rule. that is bad bad bad. 
# 3 the "+" must be escaped with a "\", as must all "." characters. 
# 4 don't put comments on the end of lines. a line like 
#		* $ $SPR^0 ^Subject:.*cows   # this matches "cows"
#	will be evaluated much differently than you think.

SPR=2147483647


### specifics:

# neuric forwarded stuff
#:0
* .*vlack\+neuric\@vlack\.com
#.INBOX.Archive.Neuric/

# spam mail for completing offers and such
#:0
#* ^X-Original-To:.*offers
#.INBOX.offerspam/

# Trash mail
* $ $SPR^0 ^To:.*vlack+spam
* $ $SPR^0 ^To:.*vlack+temp
* $ $SPR^0 ^From:.*Vanessa.*Smith
#* $ $SPR^0 ^To:.*vlack+f\.retailreportcard
* $ $SPR^0 .*vlack+f\.retailreportcard
# previously used for fusioncash.com: 
* $ $SPR^0 ^X-Original-To:.*offers 
* $ $SPR^0 ^To:.*vlack\@howe\.textdrive\.com
* $ $SPR^0 ^To:.*vlack.*marin\.joyent\.us
* $ $SPR^0 ^To:.*mledbetter\@marin\.joyent\.us
/dev/null


### my lists stuff. based on 
	## Jamie Wilkinson's http://spacepants.org/dot.procmailrc
	## and more that I don't have mentioned here mmmk
:0
* ^Delivered-To:.*vlack-lists-vlack\.com
{
	# MAILMAN REMINDERS
	# to quote young Jeffrey:
	# "I'm so completely not interested in receiving these at the end of every month"
	:0:
	* ^From: .*mailman-owner@
	* ^Subject: .* mailing list memberships reminder
	/dev/null
	
	
	## I want to rename these lists
	
	# ooh, here's a cool way to do this
	# from http://www.opensubscriber.com/message/euglug@euglug.org/2802827.html
	
	# sunhelp
	:0  
	* ^List-Id:.*<[-A-Za-z0-9]*\.sunhelp\.org>  
	* ^List-Id:.*<\/[-A-Za-z0-9]*  
	.INBOX.lists.sunhelp-${MATCH}/  
	# sgihelp - changed from @sgihelp.org to @lists.synack-hosting.com
	:0  
	* ^List-Id:.*<[-A-Za-z0-9]*\.lists\.synack-hosting\.com>  
	* ^List-Id:.*<\/[-A-Za-z0-9]*  
	.INBOX.lists.sgihelp-${MATCH}/  

	## statically-defined lists
	
	:0
	* ^Sender:.*college-owner\@oiketes.hopeoffice.org
	.INBOX.lists.hsl-college/
	:0
	* ^Sender:.*MACENTERPRISE\@lists\.psu\.edu
	.INBOX.lists.macenterprise/
        :0
        * ^List-ID: alt\.sysadmin\.recovery\.googlegroups\.com
        .INBOX.lists.asr/

	
	## announce lists can all go in the same place as far as I'm concerned
	:0 
	* $ $SPR^0 ^List-Id:.*announce.*
	* $ $SPR^0 ^Sender:.*owner.*announce.*
	* $ $SPR^0 ^Sender:.*announce.*owner.*
	* $ $SPR^0 ^List-Id:.*gentoo-gwn\.gentoo\.org
	* $ $SPR^0 ^List-Id:.*fvpr\.fridge\.net	
	* $ $SPR^0 ^List-Id:.*security-announce\.lists\.apple\.com
	* $ $SPR^0 ^List-Id:.*html\.issue\.tidbits\.com
	* $ $SPR^0 ^Subject:.*testingannouncerule.*
	.INBOX.lists.announce/
	
	## bring on the automatic list filtering. long time in coming.
	
	# generic ezmlm filter, comes before mailman otherwise mailman's filter
	# will put everything into .list.contact/
	:0
	* ^Mailing-list: contact .*; run by ezmlm
	* ^List-Post: [<]mailto:\/.*
	{
		LISTID=$MATCH
		
		:0
		* LISTID ?? ^ *\/[^@]*
		{
			LIST=$MATCH
			
			:0
			* LISTID ?? ^.*@\/[^>\.]*
			.INBOX.lists.$MATCH-$LIST/
		}
	}
	
	# onelist/egroups/yahoogroups/whatever_they're_called_today
	# lists with their non-standard header format, and ad nuker
	:0
	* ^Mailing-List: list \/[^@]+
	{
		LISTID=$MATCH
		
		:0 fbw
		| sed '/-\~-->$/,/--\~->$/d'	
		
		:0
		.INBOX.lists.$LISTID/
	}
	
	# Automagically handle nice standards conformant mailing lists - this
	# rule and the majordomo recipe appear courtesy of Jeff Waugh Records
	:0
	* ^(List-Id|(X-)?Mailing-List|X-List):\/.*
	{
		LISTID=$MATCH
		
		:0
		* LISTID ?? ^.*[<]\/[^@>\.]*
		.INBOX.lists.$MATCH/
		
		:0
		* LISTID ?? ^\/[^@\.]*
		.INBOX.lists.$MATCH/
	}
	
	# majordomo lists
	:0
	* ^Sender: owner-[^@]+@[^@]+
	* ^Sender: owner-\/[^@]+
	.INBOX.lists.$MATCH/
	
	# weird lists like college@hopeoffice.org
	#:0
	#* ^Sender: \/-owner[^@]+
	#.lists.$MATCH/
	# err... I guess I don't need this anymore? 
	
	
	# if none of these match, it should go into the lists dir
	:0
	.INBOX.lists/
}


### Filtering by address extension. This would be awesome if I could be consistent... 

## here's the original rule. if you sent something to vlack+anything@vlack.com
## then it would be put in the INBOX/anything folder. the folder delimiter was a (.)
## so vlack+anything.else would get put in INBOX/anything/else. 
## for this, I generally used 'vlack+f.something', so that all my disposable accounts
## would be inside a single hier.
	# :0
	# * ^To: vlack\+\/.*\@vlack\.com
	# * MATCH ?? ()\/[^@]+
	# .$MATCH/
## what I'm doing now is having vlack+f.anything go into my special INBOX/temp dir
## which is where temp stuff goes that if I cancel, I won't go crying. 
## (at the moment, I do have some non-trivial stuff going to f. addresses; I'll change
## this soon.)
## then, anything going to vlack+y. addresses I can put somewhere else... so that I'll
## be sure to keep those addresses which I've used around in the future. 
## finally, plain vlack+ addresses can go ... err .. somewhere. To the inbox for now. 

:0
* ^To: vlack\+
.INBOX.temp/


# use the furrin.rc filter, which deals with mail based on character encoding. I use it to
# trash spam that comes in languages I could never understand anyway. 
#:0
#{
#	INCLUDERC = "$PMDIR/furrin.rc"
#}

### The rest of the email will be delivered into my default INBOX, so no more rules are necessary
