# $Id: presencecmd.tcl 1224 2007-09-05 14:15:40Z sergei $
# "Presence commands" -- Tkabber chat plugin.
# Provides two IRC-style chat commands that provide
# for manipulating user's presence and/or assotiated status message
# as well as sending directed presence to the chat peer (or room).
# Written by Konstantin Khomoutov <flatworm@users.sourceforge.net>
# See license.terms for details on distribution.
# See INSTALL and README for details on installation and usage.

package require msgcat

namespace eval presencecmd {

    ::msgcat::mcload [file join [file dirname [info script]] msgs]

    hook::add generate_completions_hook [namespace current]::command_comps
    hook::add chat_send_message_hook [namespace current]::handle_command 15
}

proc presencecmd::command_comps {chatid compsvar wordstart line} {
    upvar 0 $compsvar comps
    
    if {!$wordstart} {
	lappend comps {/presence } {/chatpresence } {/thispresence }
    }
}

proc presencecmd::handle_command {chatid user body type} {
    global userstatus

    if {[string match {/presence*} $body]} {
	set cmd /presence
    } elseif {[string match {/chatpresence*} $body]} {
	set cmd /chatpresence
    } elseif {[string match {/thispresence*} $body]} {
	set cmd /thispresence
    } else return

    set fields [split $body \n]
    set pres   [string trim [string range [lindex $fields 0] [string length $cmd] end]]
    set status [string trim [join [lrange $fields 1 end] \n]]
    set sendstatus [expr {$status != ""}]

    if {$pres == "" && !$sendstatus} {
	show_usage $chatid
	return stop
    }

    if {$pres != ""} {
	switch -- $pres {
	    avail {
		set pres available
	    }
	    available -
	    away -
	    xa -
	    dnd -
	    chat {
	    }
	    clear -
	    clearstatus {
		set sendstatus true
		set pres $userstatus
		set status ""
	    }
	    default {
		show error $chatid [::msgcat::mc "Unknown presence \"%s\".\
		    Must be avail\[able\], away, xa, dnd, chat or clear\[status\]" $pres]
		return stop
	    }
	}
    } else {
	set pres $userstatus
    }

    switch -- $cmd {
	/presence {
	    set_master_presence $pres $status $sendstatus
	}
	/chatpresence -
	/thispresence {
	    send_directed_presence $chatid $pres $status $sendstatus
	}
    }

    return stop
}

proc presencecmd::set_master_presence {pres status sendstatus} {
    global userstatus textstatus

    if {$sendstatus} {
	set textstatus $status
    }

    # the following assignment triggers sending the presence
    set userstatus $pres
}

proc presencecmd::send_directed_presence {chatid pres status sendstatus} {
    global userpriority

    set cmd [list send_presence $pres \
		      -to [chat::get_jid $chatid] \
		      -pri $userpriority \
		      -connection [chat::get_connid $chatid]]

    if {$sendstatus} {
	lappend cmd -stat $status
    }

    eval $cmd
}

proc presencecmd::show_usage chatid {
    show error $chatid [::msgcat::mc "Usage:\
	\t/presence ?presence|clear\[status\]?\n\
	\t?status message?\n\
	or\n\
	\t/chatpresence ?presence|clear\[status\]?\n\
	\t?status message?\n\
	Where presence is one of: avail\[able\], away, xa, dnd, chat.\n\
	Special presence \"clear\[status\]\" just clears the current status.\n\
	/thispresence is an alias for /chatpresence"]
}

# $type should be either "info" or "error"
proc presencecmd::show {type chatid msg} {
    set jid [chat::get_jid $chatid]
    set cw [chat::chat_win $chatid]

    chat::add_message $chatid $jid $type $msg {}
}

# vim:ts=8:sw=4:sts=4:noet
