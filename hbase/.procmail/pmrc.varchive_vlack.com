SHELL = /usr/local/bin/bash

MAILDIR=$HOME/Maildir/ 
DEFAULT=$MAILDIR 
LOGFILE=$HOME/.procmail.log
LOGABSTRACT = "all"
VERBOSE = "on"

PMDIR=$HOME/.procmail
#DOMAIN=howe\.textdrive\.com

SPR=2147483647

### my lists stuff. based on 
	## Jamie Wilkinson's http://spacepants.org/dot.procmailrc
	## and more that I don't have mentioned here mmmk
:0
* ^Delivered-To:.*vlack-lists-vlack\.com
{
	# MAILMAN REMINDERS
	# to quote young Jeffrey:
	# "I'm so completely not interested in receiving these at
	# the end of every month"
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



### Delete EVERYTHING
:0 
/dev/null


