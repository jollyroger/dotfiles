# Tkabber presence spy module, version 1.0
# Copyright (C) 2003 Maciek Pasternacki <maciekp@japhy.fnord.org>
#
# Traces presence messages, logging them into tkabber window
# and/or file.
#

option add *Spy.timestampforeground Black         widgetDefault
option add *Spy.nickforeground      DarkBlue      widgetDefault
option add *Spy.jidforeground       DarkBlue      widgetDefault
option add *Spy.presenceforeground  DarkRed       widgetDefault
option add *Spy.reasonforeground    DarkMagenta   widgetDefault

package require msgcat

namespace eval spy {
    set the_file ""
    set watches {}

    variable options

    variable scriptdir [file dirname [info script]]
    ::msgcat::mcload [file join $scriptdir msgs]

    custom::defgroup Plugins [::msgcat::mc "Plugins options."] \
	-group Tkabber
    custom::defgroup Spy [::msgcat::mc "Spy Presence plugin options."] \
	-group Plugins

    custom::defvar options(log_file) "" \
	[::msgcat::mc "File to log Spy Presence messages."] \
	-type string -group Spy

    custom::defvar options(timestamp_format) {[%m/%d %R]} \
	[::msgcat::mc "Format of timestamp in Spy Presence window."] \
	-type string -group Spy
}

proc spy::open_window {} {
    variable watches

    set w .spy
    if {[winfo exists $w]} {
	raise_win $w
        return
    }

    add_win $w -title [::msgcat::mc "Presence Spy"] \
	       -tabtitle [::msgcat::mc "Spy"] \
	       -class Spy \
	       -raisecmd [list focus $w.log] \
	       -raise 1

    frame $w.controls
    button $w.controls.add -text [::msgcat::mc "Set watch"] \
      -command "[namespace current]::set_watch"

    set watch_entry [entry $w.controls.watch_regex]
    bind $watch_entry <Return> "[namespace current]::set_watch"
    pack $w.controls.add -side left -anchor e
    pack $watch_entry -side left -anchor w -fill x
    pack $w.controls -side bottom -fill x

    frame $w.watches
    foreach watch $watches {
        set watch_id [lindex $watch 0]
        set watch_regex [lindex $watch 1]
        add_watch_frame $watch_id $watch_regex
    }
    pack $w.watches -side bottom -fill x
    
    set sw [ScrolledWindow $w.isw -scrollbar vertical]
    pack $sw -side top -fill both -expand yes -in $w

    set log [text $w.log -wrap word]
    $sw setwidget $log

    $log configure -state disabled -takefocus 1
    bind $log <1> [list focus $log]

    $log tag configure timestamp \
	-foreground [option get $w timestampforeground Spy]
    $log tag configure nick \
	-foreground [option get $w nickforeground Spy]
    $log tag configure jid \
	-foreground [option get $w jidforeground Spy]
    $log tag configure presence \
	-foreground [option get $w presenceforeground Spy]
    $log tag configure reason \
	-foreground [option get $w reasonforeground Spy]

    search::setup_panel $w
}

proc spy::set_watch {} {
    variable watches
    
    set w .spy

    if {![winfo exists $w]} return
    set entr $w.controls.watch_regex
    set regex [$entr get]

    if { $regex == "" } return

    # Add new watch if there is no such already present
    set next_watch [expr [llength $watches] + 1]
    foreach watch $watches {
	set r [lindex $watch 1]
	set idx [lindex $watch 0]
	if {$r == $regex} {
	    #set b [$w.watches.$idx cget -background]
	    #$w.watches.$idx configure -background red
	    #after 1500 [list $w.watches.$idx configure -background $b]
	    return
	}
    }
    if {[catch { regexp $regex "" }]} {
	$entr configure -fg [option get $entr errorForeground Entry]
	return
    } else {
	$entr configure -fg [option get $entr foreground Entry]
    }
    lappend watches [list $next_watch $regex]
    add_watch_frame $next_watch $regex
}

proc spy::add_watch_frame {watch_id watch_regex} {
    variable spy_alerts
    set w .spy

    if {![winfo exists $w]} return

    frame $w.watches.$watch_id
    button $w.watches.$watch_id.remove -text [::msgcat::mc "Remove"] \
      -command [list [namespace current]::remove_watch $watch_id $watch_regex]
    label $w.watches.$watch_id.regex -text $watch_regex \
      -foreground [option get $w nickforeground Spy]
    label $w.watches.$watch_id.nick -text "" \
      -foreground [option get $w nickforeground Spy]
    label $w.watches.$watch_id.jid -text "" \
      -foreground [option get $w nickforeground Spy]
    label $w.watches.$watch_id.timestamp -text "" \
      -foreground [option get $w timestampforeground Spy]
    label $w.watches.$watch_id.presence -text "" \
      -foreground [option get $w presenceforeground Spy]
    label $w.watches.$watch_id.reason -text "" \
	  -foreground [option get $w reasonforeground Spy]
    checkbutton $w.watches.$watch_id.alert -text [::msgcat::mc "Alert when available"] \
      -variable [namespace current]::spy_alerts($watch_id)

    set [namespace current]::spy_alerts($watch_id) 0

    pack $w.watches.$watch_id.remove \
      $w.watches.$watch_id.regex \
      $w.watches.$watch_id.timestamp \
      $w.watches.$watch_id.nick \
      $w.watches.$watch_id.jid \
      $w.watches.$watch_id.presence -side left
    pack $w.watches.$watch_id.alert -side right
    pack $w.watches.$watch_id.reason -side left -fill x
    pack $w.watches.$watch_id -side bottom -fill x

}

proc spy::remove_watch {watch_id watch_regex} {
    variable watches

    set w .spy.watches.$watch_id
    if {![winfo exists $w]} return

    set idx [lsearch -exact $watches [list $watch_id $watch_regex]]
    set watches [lreplace $watches $idx $idx]
    destroy $w
}

proc spy::update_watch {connid watch_id nick jid type reason} {
    variable options
    variable spy_alerts
    set w .spy.watches.$watch_id
    if {![winfo exists $w]} return

    $w.timestamp configure -text \
	[clock format [clock seconds] -format $options(timestamp_format)]
    $w.nick configure -text $nick
    $w.jid configure -text $jid
    $w.presence configure -text $type
    $w.reason configure -text $reason

    if { $spy_alerts($watch_id) && $type=="available" } {
        alert_dialog $connid $watch_id $nick $jid
    }
}

proc spy::alert_dialog {connid watch_id nick jid} {
    set w .spy_alert_${watch_id}

    if {[winfo exists $w]} {
        destroy $w
    }

    set message [format [::msgcat::mc "Spy Alert: user %s (%s) is available!"] $nick $jid]
    Dialog $w -title $message -modal none -separator 1 -anchor e -default 0 -cancel 1

    set frame [$w getframe]
    message $frame.msg -text $message -width 70c
    pack $frame.msg -side left -fill x

    $w add -text [::msgcat::mc "Open chat"] -command [list [namespace current]::open_chat $w $connid $jid ]
    $w add -text [::msgcat::mc "Cancel"] -command [list destroy $w]

    $w draw
}

proc spy::open_chat {w connid jid} {
    destroy $w

    chat::open_to_user $connid $jid
}


proc spy::display {connid jid nick type reason} {
    variable options
    variable watches
    set w .spy

    if {![winfo exists $w]} return
    set log $w.log

    $log configure -state normal

    $log insert end \
	[clock format [clock seconds] -format $options(timestamp_format)] timestamp
    $log insert end " "

    if { "$nick"!="" } {
        $log insert end "$nick" nick
        $log insert end " ("
        $log insert end "$jid" jid
        $log insert end ") "
    } else {
        $log insert end "$jid" jid
        $log insert end " "
    }

    $log insert end "$type" presence

    if { "$reason"!="" } {
        $log insert end " ("
        $log insert end "$reason" reason
        $log insert end ")"
    }
    $log insert end "\n"
    $log see end

    $log configure -state disabled

    foreach watch $watches {
        set watch_id [lindex $watch 0]
        set watch_regex [lindex $watch 1]
        if {[regexp "$watch_regex" $nick temp] || [regexp "$watch_regex" $jid temp]} {
            update_watch $connid $watch_id $nick $jid $type $reason
        }
    }
}

proc spy::client_presence_handler {connid from type x args} {
    variable options
    variable the_file

    if {[catch { chat::get_nick $connid $from chat } nick]} {
	if {[catch { chat::get_nick $from chat } nick]} {
	    set nick $from
	}
    }

    if { "$nick"=="$from" } {
        set nick ""
    }

    set reason ""
    set show ""
    foreach {attr val} $args {
	switch -- $attr {
	    -status   {set reason $val}
	    -show     {set show   $val}
	}
    }

    if { "$type"=="" } { set type "available" }
    if { "$show"!="" } { set type "$type/$show" }

    display "$connid" "$from" "$nick" "$type" "$reason"

    if {$options(log_file) != "" && $the_file == ""} {
	init_spy
    }

    if {$options(log_file) == "" && $the_file != ""} {
	deinit_spy
    }

    if {$the_file != ""} {
        puts -nonewline $the_file "[clock format [clock seconds]] "
        if {$nick != ""} {
            puts -nonewline $the_file "$nick ($from) "
        } else {
            puts -nonewline $the_file "$from "
        }
        puts -nonewline $the_file "$type"
        if {$reason != ""} {
            puts -nonewline $the_file " ($reason)"
        }
        puts $the_file {}
        flush $the_file
    }
}

proc spy::init_spy {} {
    variable the_file
    variable options

    if {$options(log_file) != ""} {
        if {$the_file != ""} {
            catch { close $the_file }
            set the_file ""
        }
        set the_file [open $options(log_file) a]
        puts $the_file "[clock format [clock seconds]] Started spying."
    }
}

proc spy::deinit_spy {} {
    variable the_file
    if {$the_file != ""} {
        puts $the_file "[clock format [clock seconds]] Spy goes away."
        catch { close $the_file }
        set the_file ""
    }
}

proc spy::setup_menu {} {
    catch { 
	set m [.mainframe getmenu plugins]

        $m add command -label [::msgcat::mc "Spy presence"] \
	    -command [namespace current]::open_window
    }
}

hook::add postload_hook [namespace current]::spy::init_spy 100
hook::add client_presence_hook [namespace current]::spy::client_presence_handler 100
hook::add quit_hook [namespace current]::spy::deinit_spy 100
hook::add finload_hook [namespace current]::spy::setup_menu

namespace eval spy::search {}

proc spy::search::open_panel {w sf} {
    pack $sf -side bottom -anchor w -fill x -before $w.isw
    update idletasks
    $w.log see end
}

proc spy::search::close_panel {w sf} {
    $w.log tag remove search_highlight 0.0 end
    pack forget $sf
    focus $w.log
}

proc spy::search::setup_panel {w} {
    set log $w.log

    $log mark set sel_start end
    $log mark set sel_end 0.0

    set sf [plugins::search::spanel [winfo parent $log].search \
		-searchcommand [list ::plugins::search::do_text_search $log] \
		-closecommand [list [namespace current]::close_panel $w]]

    bind $w.log <<OpenSearchPanel>> \
	[double% [list [namespace current]::open_panel $w $sf]]
}

##############################################################################

proc spy::restore_window {args} {
    open_window
}

proc spy::save_session {vsession} {
    upvar 2 $vsession session
    global usetabbar

    # We don't need JID at all, so make it empty (special case)
    set user     ""
    set server   ""
    set resource ""

    # TODO
    if {!$usetabbar} return

    set prio 0
    foreach page [.nb pages] {
	set path [ifacetk::nbpath $page]

	if {[string equal $path .spy]} {
	    lappend session [list $prio $user $server $resource \
		[list [namespace current]::restore_window] \
	    ]
	}
	incr prio
    }
}

hook::add save_session_hook [namespace current]::spy::save_session

