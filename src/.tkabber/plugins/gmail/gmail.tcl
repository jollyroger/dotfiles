# Gmail notifications support.

package require msgcat

namespace eval gmail {

    ::msgcat::mcload [file join [file dirname [info script]] msgs]

    set ::NS(gnotify) "google:mail:notify"

    custom::defgroup Plugins \
	[::msgcat::mc "Plugins options."] \
	-group Tkabber
    
    custom::defgroup {Gmail Notifications} \
	[::msgcat::mc "Google Talk XMPP extensions."] \
	-group Plugins
    
    custom::defvar options(gmail_notifications) 1 \
	[::msgcat::mc "Request Gmail notifications."] \
	-type boolean -group {Gmail Notifications} \
	-command [namespace current]::request_all_notifications

    custom::defvar options(delete_old_notifications) 1 \
	[::msgcat::mc "Delete Gmail notifications, which are older than 24 hours."] \
	-type boolean -group {Gmail Notifications} \
	-command [namespace current]::request_all_notifications

    custom::defvar options(timestamp_format) {[%m/%d %R] } \
	[::msgcat::mc "Format of timestamp in Gmail tree view. Set to\
		       empty string if you don't want to see timestamps."] \
	-group {Gmail Notifications} -type string

    custom::defvar last_mail_time {} \
	[::msgcat::mc "Last Gmail message time."] \
	-type string -group Hidden
}

############################################################################

proc gmail::request_all_notifications {args} {
    foreach connid [jlib::connections] {
	request_notifications $connid
    }
}

############################################################################

proc gmail::request_notifications {connid} {
    variable options
    variable last_mail_time

    set jid [jlib::connection_bare_jid $connid]
    catch {array set tmp $last_mail_time}

    if {[info exists tmp($jid)]} {
	set time $tmp($jid)
    } else {
	set time 0
    }

    if {$options(gmail_notifications)} {
	jlib::send_iq get \
	    [jlib::wrapper:createtag query \
		 -vars [list xmlns $::NS(gnotify) \
			     newer-than-time $time]] \
	    -command [list [namespace current]::receive_notifications $jid] \
	    -connection $connid
    }
}

hook::add connected_hook [namespace current]::gmail::request_notifications

############################################################################

proc gmail::receive_notifications {jid res child} {
    variable last_mail_time

    if {$res != "OK"} {
	return
    }

    jlib::wrapper:splitxml $child tag vars isempty cdata children

    if {[jlib::wrapper:isattr $vars result-time]} {
	catch {array set tmp $last_mail_time}
	set tmp($jid) [jlib::wrapper:getattr $vars result-time]
	set last_mail_time [array get tmp]
    }

    fill_tree $jid $children
}

#############################################################################

proc gmail::create_menu {} {
    set menu [.mainframe getmenu plugins]
    $menu add command \
	  -label [::msgcat::mc "Open Gmail notifications"] \
	  -command [list [namespace current]::open_window -raise 1]
}

hook::add finload_hook [namespace current]::gmail::create_menu

#############################################################################

proc gmail::open_window {args} {
    global tcl_platform
    variable options

    set raise 0
    foreach {key val} $args {
	switch -- $key {
	    -raise { set raise $val }
	}
    }

    set w .gmail_messages

    if {[winfo exists $w]} {
	if {$raise} {
	    raise_win $w
	}
	return
    }

    add_win $w -title [::msgcat::mc "Gmail notifications"] \
	-tabtitle [::msgcat::mc "Gmail"] \
	-raisecmd [list focus $w.tree] \
	-class JDisco \
	-raise $raise

    if {![info exists options(seencolor)]} {
	if {[cequal $tcl_platform(platform) unix] && \
		![string equal [option get $w disabledForeground JDisco] ""]} {
	    set options(seencolor) [option get $w disabledForeground JDisco]
	} else {
	    set options(seencolor) [option get $w featurecolor JDisco]
	}
    }
    if {![info exists options(unseencolor)]} {
	set options(unseencolor) [option get $w fill JDisco]
    }

    set sw [ScrolledWindow $w.sw]
    set tw [Tree $w.tree -deltax 16 -deltay 18 -dragenabled 0]
    $sw setwidget $tw

    pack $sw -side top -expand yes -fill both

    $tw bindText <ButtonPress-3> \
	    [list [namespace current]::message_popup $tw]
    $tw bindText <Double-ButtonPress-1> \
	    [list [namespace current]::message_action browse $tw]

    # HACK
    bind $tw.c <Return> \
         "[namespace current]::message_action browse $tw \[$tw selection get\]"
    bindscroll $tw.c

    messages_restore
}

#############################################################################

proc gmail::fill_tree {jid children} {
    variable options

    if {[lempty $children]} {
	return
    }
    
    open_window

    foreach ch $children {
	jlib::wrapper:splitxml $ch tag1 vars1 isempty1 cdata1 children1

	switch -- $tag1 {
	    mail-thread-info {
		set tid [jlib::wrapper:getattr $vars1 tid]
		set messages [jlib::wrapper:getattr $vars1 messages]
		set date [jlib::wrapper:getattr $vars1 date]
		set url [jlib::wrapper:getattr $vars1 url]
		add_thread $jid $tid $messages $date $url $children1 1
	    }
	}
    }
}

package require md5

proc gmail::add_thread {jid tid messages date url children unseen} {
    variable options

    set w .gmail_messages
    set tw $w.tree

    set fnode [str2node $jid]
    if {![$tw exists $fnode]} {
	$tw insert end root $fnode -text $jid -open 1 \
	    -fill $options(unseencolor) -image browser/user \
	    -data [list type jid jid $jid unseen $unseen]
    }

    set senders [list]
    set subject ""
    foreach ch $children {
	jlib::wrapper:splitxml $ch tag1 vars1 isempty1 cdata1 children1

	switch -- $tag1 {
	    senders {
		foreach ch1 $children1 {
		    jlib::wrapper:splitxml $ch1 tag2 vars2 isempty2 cdata2 children2
		    if {$tag2 == "sender"} {
			lappend senders [jlib::wrapper:getattr $vars2 name]
		    }
		}
	    }
	    subject {
		set subject $cdata1
	    }
	}
    }
    
    set snode [str2node "$tid $jid"]
    if {[$tw exists $snode]} {
	$tw delete $snode
    }

    set timestamp [clock format [string range $date 0 end-3] -format $options(timestamp_format)]
    set names [senders2names $senders]
    $tw insert end $fnode $snode \
	-text "$timestamp$names ($messages) $subject" -open 1 \
	-fill $options(unseencolor) \
	-data [list type thread jid $jid tid $tid \
		    messages $messages date $date url $url \
		    children $children unseen $unseen]

    if {$options(delete_old_notifications)} {
	message_action deleteold $tw $fnode
    } else {
	messages_store $tw
    }
    message_update $tw $snode
    sort_nodes $tw $fnode -date
    tab_set_updated $w 1 message
}

proc gmail::senders2names {senders} {
    if {[llength $senders] <= 1} {
	return [lindex $senders 0]
    } else {
	set names {}
	foreach s $senders {
	    lappend names [lindex [split [string trim $s]] 0]
	}
	if {[llength $names] <= 3} {
	    return [join $names ", "]
	} else {
	    return "[lindex $names 0] .. [join [lrange $names end-1 end] {, }]"
	}
    }
}

proc gmail::str2node {string} {
    set utf8str [encoding convertto utf-8 $string]
    if {[catch { ::md5::md5 -hex $utf8str } ret]} {
	return [::md5::md5 $utf8str]
    } else {
	return $ret
    }
}

proc gmail::message_popup {tw node} {
    $tw selection set $node

    if {[catch { array set props [$tw itemcget $node -data] }]} {
	return
    }

    set m .gmail_popup_menu

    if {[winfo exists $m]} {
	destroy $m
    }

    menu $m -tearoff 0

    switch -- $props(type) {
	jid {
	#    $m add command -label [::msgcat::mc "Mark all seen"] \
	#	-command [list [namespace current]::message_action markseen $tw $node]
	#    $m add command -label [::msgcat::mc "Mark all unseen"] \
	#	-command [list [namespace current]::message_action markunseen $tw $node]
	    $m add command -label [::msgcat::mc "Delete messages older than 24 hours"] \
		-command [list [namespace current]::message_action deleteold $tw $node]
	#    $m add command -label [::msgcat::mc "Delete seen messages"] \
	#	-command [list [namespace current]::message_action deleteseen $tw $node]
	    $m add command -label [::msgcat::mc "Delete all messages"] \
		-command [list [namespace current]::message_action delete $tw $node]
	}
	thread {
	    $m add command -label [::msgcat::mc "Browse"] \
		-command [list [namespace current]::message_action browse $tw $node]
	#    $m add command -label [::msgcat::mc "Mark seen"] \
	#	-command [list [namespace current]::message_action markseen $tw $node]
	#    $m add command -label [::msgcat::mc "Mark unseen"] \
	#	-command [list [namespace current]::message_action markunseen $tw $node]
	    $m add command -label [::msgcat::mc "Delete"] \
		-command [list [namespace current]::message_action delete $tw $node]
	}
	default {
	    return
	}
    }

    tk_popup $m [winfo pointerx .] [winfo pointery .]
}

proc gmail::message_action {action tw node} {
    message_action_aux $action $tw $node
    messages_store $tw
}

proc gmail::message_action_aux {action tw node} {
    variable options

    if {[catch { array set props [$tw itemcget $node -data] }]} {
        return
    }

    switch -glob -- $props(type)/$action {
	jid/markseen {
            foreach child [$tw nodes $node] {
		message_action_aux markseen $tw $child
            }
	}
	jid/markunseen {
            foreach child [$tw nodes $node] {
		message_action_aux markunseen $tw $child
            }
	}
	jid/deleteold {
	    foreach child [$tw nodes $node] {
		message_action_aux deleteold $tw $child
	    }
	}
	jid/deleteseen {
	    foreach child [$tw nodes $node] {
		message_action_aux deleteseen $tw $child
	    }
	}
	jid/delete {
	    foreach child [$tw nodes $node] {
		message_action_aux delete $tw $child
	    }
	}
	thread/browse {
	    if {$props(url) != ""} {
		browseurl $props(url)
	    }
	}
	thread/markseen {
            set props(unseen) 0
	}
	thread/markunseen {
            set props(unseen) 1
	}
	thread/deleteold {
	    set datediff [expr {[clock seconds] - [string range $props(date) 0 end-3]}]
	    if {$datediff > 86400} {
		message_action_aux delete $tw $node
	    }
	}
	thread/deleteseen {
	    if {!$props(unseen)} {
		message_action_aux delete $tw $node
	    }
	}
	thread/delete {
            set props(unseen) 0
            $tw itemconfigure $node -data [array get props]
            message_update $tw $node

	    # Deduce the node to select after $node is deleted:
	    # Next sibling is tried first, then previous, then parent node.
	    set p [$tw parent $node]
	    set end [expr {[llength [$tw nodes $p]] - 1}]
	    set ix [$tw index $node]
	    if {$ix < $end} {
		set next [$tw nodes $p [incr ix]]
	    } elseif {$ix > 0} {
		set next [$tw nodes $p [incr ix -1]]
	    } else {
		set next $p
	    }

            $tw delete $node

	    if {![string equal $next root]} {
		$tw selection set $next
	    }
	}
	default {
	    return
	}
    }
}

proc gmail::sort_nodes {tw node type} {
    if {[string range $type 0 0] == "-"} {
	set order -decreasing
	set type [string range $type 1 end]
    } elseif {[string range $type 0 0] == "+"} {
	set order -increasing
	set type [string range $type 1 end]
    } else {
	set order -increasing
    }

    set children {}
    foreach child [$tw nodes $node] {
        catch { unset props }
        array set props [$tw itemcget $child -data]

	lappend children [list $child $props($type)]
    }
    set neworder {}
    foreach child [lsort $order -index 1 $children] {
        lappend neworder [lindex $child 0]
    }
    $tw reorder $node $neworder
}

proc gmail::message_update {tw node} {
    variable options

    for {set parent [$tw parent $node]} \
            {![cequal $parent root]} \
            {set parent [$tw parent $parent]} {
        set unseen 0

        foreach child [$tw nodes $parent] {
            catch { unset props }
            array set props [$tw itemcget $child -data]

            incr unseen $props(unseen)
        }

        catch { unset props }
        array set props [$tw itemcget $parent -data]
        set props(unseen) $unseen

        set text $props(jid)
        set myfill $options(seencolor)
        if {$unseen > 0} {
            append text " ($unseen)"
            set myfill $options(unseencolor)
        }
        $tw itemconfigure $parent -text $text -fill $myfill \
                -data [array get props]
    }
}

#############################################################################

proc gmail::messages_store {tw} {
    set file [file join $::configdir gmail-notifications.tcl]
    set file0 [file join $::configdir gmail-notifications0.tcl]
    set file1 [file join $::configdir gmail-notifications1.tcl]

    if {[catch {open $file1 {WRONLY CREAT TRUNC}} fd]} {
        debugmsg plugins "unable to open $file1: $fd"
        return
    }
    fconfigure $fd -encoding utf-8

    set code [catch {messages_store_aux $tw root $fd} result]

    catch {close $fd}

    if {$code} {
        debugmsg plugins $result
        catch {file delete $file1}
        return
    }

    set renameP 0
    if {![file exists $file]} {
    } elseif {[file size $file] == 0} {
        catch {file delete -force $file}
    } else {
        set renameP 1
        catch {file rename -force $file $file0}
    }

    if {![catch {file rename $file1 $file} result]} {
        return
    }
    debugmsg plugins "unable to rename $file1 to $file: $result"

    if {($renameP) && ([catch {file rename -force $file0 $file} result])} {
        debugmsg plugins "unable to rename $file0 back to $file: $result"
    }
    catch {file delete $file1}

    return
}

#############################################################################

proc gmail::messages_store_aux {tw node fd} {
    if {![winfo exists $tw]} {
        return
    }

    if {[llength [set children [$tw nodes $node]]] > 0} {
        foreach child $children {
            messages_store_aux $tw $child $fd
        }
    } elseif {![catch {array set props [$tw itemcget $node -data]}]} {
        puts $fd [list [namespace current]::add_thread \
		       $props(jid) $props(tid) $props(messages) \
		       $props(date) $props(url) $props(children) \
		       $props(unseen)]
    }
}

#############################################################################

proc gmail::messages_restore {} {
    set file [file join $::configdir gmail-notifications.tcl]
    if {[file exists $file]} {
	catch {
	    set fd [open $file "r"]
	    fconfigure $fd -encoding utf-8
	    uplevel #0 [read $fd]
	    close $fd
	}
    }

    return ""
}

#############################################################################

proc gmail::notify_response {connid from lang child} {
    variable options

    if {$from != "" && \
	    $from != [jlib::connection_bare_jid $connid] && \
	    $from != [jlib::connection_jid $connid]} {
	return {error cancel not-allowed}
    }

    jlib::wrapper:splitxml $child tag vars isempty cdata children

    if {$tag != "new-mail"} {
	return {error modify bad-request}
    }

    request_notifications $connid

    return [list result ""]
}

iq::register_handler set "" $::NS(gnotify) \
    [namespace current]::gmail::notify_response

#############################################################################

proc gmail::restore_window {from connid jid} {
    open_window -raise 1
}

#############################################################################

proc gmail::save_session {vsession} {
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

	if {[string equal $path .gmail_messages]} {
	    lappend session \
		    [list $prio $user $server $resource \
			  [list [namespace current]::restore_window ""]]
	}
	incr prio
    }
}

hook::add save_session_hook [namespace current]::gmail::save_session

#############################################################################

