# $Id: attline.tcl 1349 2008-01-20 19:49:12Z sergei $
# "Attention line" -- chat plugin for Tkabber.
# Draws horizontal line in chat windows separating read and unread messages.
# Written by Konstantin Khomoutov <flatworm@users.sourceforge.net>
# See license.terms for the terms of distribution.

package require msgcat

option add *Chat.attentionLineHeight        1     widgetDefault
option add *Chat.attentionLineColor         black widgetDefault
option add *Chat.attentionLinePadX          5     widgetDefault
option add *Chat.attentionLinePadY          0     widgetDefault

namespace eval attline {
    variable state
    variable options

    proc my what {
	return [uplevel 1 namespace current]::$what
    }
    proc mycmd args {
	lset args 0 [uplevel 1 namespace current]::[lindex $args 0]
    }

    ::msgcat::mcload [file join [file dirname [info script]] msgs]

    custom::defgroup Plugins [::msgcat::mc "Plugins options."] -group Tkabber

    custom::defgroup {Attention Line} \
	[::msgcat::mc "Attention Line chat plugin options.\
		       This plugin draws horizontal line separating\
		       read and unread messages in chat windows."] \
	-group Plugins \
	-group Chat

    custom::defvar options(expires_after) 1000 \
	[::msgcat::mc "Time (in milliseconds) after which unread messages\
		       in the currently active chat window are considered read\
		       and the attention line is considered expired."] \
	-group {Attention Line} \
	-type integer

    custom::defvar options(remove_expired) false \
	[::msgcat::mc "Remove the attention line after it was expired\
		       from its chat window."] \
	-group {Attention Line} \
	-type boolean

    hook::add open_chat_post_hook [mycmd setup_chat_win]
    # must perform after the hook from 'log on open' plugin:
    hook::add open_chat_post_hook [mycmd draw_chat_history_separator] 101

    hook::add close_chat_post_hook [mycmd cleanup]

    # must perform earlier than drawing of timestamp:
    hook::add draw_message_hook [mycmd on_draw_message] 5.5

    hook::add got_focus_hook  [mycmd on_focused]
    hook::add lost_focus_hook [mycmd on_lost_focus]
}

proc attline::attline {cw} {
    return $cw.attention_line
}

proc attline::unread {cw {val ""}} {
    variable state
    if {$val == ""} {
	return $state($cw,unread)
    } else {
	set state($cw,unread) $val
    }
}

proc attline::atbottom {cw {val ""}} {
    variable state
    if {$val == ""} {
	return $state($cw,atbottom)
    } else {
	set state($cw,atbottom) $val
    }
}

proc attline::isvisible {text index} {
    expr {[llength [$text bbox $index]] > 0}
}

proc attline::setup_chat_win {chatid type} {
    variable state
    set cw [chat::chat_win $chatid]
    set iw [chat::input_win $chatid]

    set state($cw,mainwindow) [chat::winid $chatid]

    #unread $cw [expr {![has_focus $chatid]}]
    unread $cw false
    atbottom $cw false

    bind $iw <<ChatSeeAttentionLine>> +[mycmd see_attention_line $cw]

    return
}

proc attline::cleanup {chatid} {
    variable state

    set cw [chat::chat_win $chatid]

    cancel_attline_expiration $cw

    unset state($cw,mainwindow)
    unset state($cw,unread)
    unset state($cw,atbottom)
}

proc attline::getopt {cw opt} {
    variable state

    chat::query_optiondb $state($cw,mainwindow) $opt
}

proc attline::on_draw_message {chatid from type body x} {
    if {[is_delayed $x]} return

    set cw [chat::chat_win $chatid]

    if {![has_focus $chatid] && ![unread $cw]} {
	unread $cw true
	if {[drawn $cw]} {
	    redraw_attention_line $cw
	} else {
	    draw_attention_line $cw
	}
    }
    atbottom $cw false

    return
}

proc attline::is_delayed {xml} {
    foreach xelem $xml {
	::jlib::wrapper:splitxml $xelem tag vars isempty chdata children
	switch -- [::jlib::wrapper:getattr $vars xmlns] {
	    urn:xmpp:delay -
	    jabber:x:delay {
		return 1
	    }
	}
    }
    return 0
}

proc attline::drawn {cw} {
    winfo exists [attline $cw]
}

proc attline::draw_chat_history_separator {chatid type} {
    if {[string equal $type chat]} {
	set cw [chat::chat_win $chatid]
	# Draw only if text widget isn't empty (has some history lines):
	if {[$cw compare 1.0 < end-1c]} {
	    draw_attention_line $cw
	}
    }

    return
}

proc attline::draw_attention_line {cw} {
    set al [attline $cw]

    frame $al
    bind $cw <Configure> +[mycmd reconfigure_attention_line $cw $al]
    # Prevent destructed attention line from killing its parent
    # in windowed mode when there's no explicit handler and the
    # event is forwarded upstream:
    bind $al <Destroy> +break

    $cw window create end -window $al

    reconfigure_attention_line $cw $al

    debugmsg attline "drawn"
}

proc attline::delete_attention_line {cw} {
    set state [$cw cget -state]
    $cw configure -state normal
    $cw delete [attline $cw]
    $cw configure -state $state

    debugmsg attline "deleted"
}

proc attline::redraw_attention_line {cw} {
    set al [attline $cw]
    set ix [$cw index $al]

    if {[atbottom $cw]} {
	debugmsg attline "at bottom, won't redraw"
	return
    }

    set state [$cw cget -state]
    $cw configure -state normal

    $cw window configure $ix -window {}
    $cw delete $ix

    $cw window create end -window $al

    reconfigure_attention_line $cw $al

    $cw configure -state $state

    debugmsg attline "redrawn"
}

proc attline::see_attention_line {cw} {
    set al [attline $cw]
    if {[winfo exists $al] && ![isvisible $cw $al]} {
	$cw see $al
    }
}

proc attline::internal_width {cw} {
    # We assume $cw is mapped...
    expr { [winfo width $cw]
	- 2 * [$cw cget -borderwidth]
	- 2 * [$cw cget -padx]
	- 2 * [$cw cget -highlightthickness]
    }
}

proc attline::reconfigure_attention_line {cw al} {
    if {![winfo exists $al]} return

    set padx [getopt $cw attentionLinePadX]
    $al configure \
	-background [getopt $cw attentionLineColor] \
	-height     [getopt $cw attentionLineHeight] \
	-width      [expr {[internal_width $cw] - 2 * $padx }]
    $cw window configure $al \
	-padx       $padx \
	-pady       [getopt $cw attentionLinePadY] \
}

proc attline::has_focus {chatid} {
    global usetabbar

    if {$usetabbar} {
	expr {![string equal [focus -displayof .] ""] && \
	    [string equal [chat::winid $chatid] [ifacetk::nbpath [.nb raise]]]}
    } else {
	set fw [focus -displayof .]
	expr {![string equal $fw ""] && \
	    [string equal [winfo toplevel $fw] [chat::winid $chatid]]}
    }
}

proc attline::on_focused {w} {
    set chatid [chat::winid_to_chatid $w]
    if {$chatid == ""} return

    set cw [chat::chat_win $chatid]
    debugmsg attline "focused; unread? [unread $cw]"
    if {[unread $cw]} {
	see_attention_line $cw
	schedule_attline_expiration $cw
    }
}

proc attline::on_lost_focus {w} {
    set chatid [chat::winid_to_chatid $w]
    if {$chatid == ""} return

    set cw [chat::chat_win $chatid]
    debugmsg attline "lost focus; unread? [unread $cw]"
    if {[unread $cw]} {
	cancel_attline_expiration $cw
    } elseif {[drawn $cw]} {
	redraw_attention_line $cw
	atbottom $cw true
    }
}

proc attline::schedule_attline_expiration {cw} {
    variable state
    variable options

    set exptime $options(expires_after)

    if {$exptime <= 0} {
	# Immediate expiration:
	unread $cw false
	debugmsg attline "expired immediately"
	return
    }

    set state($cw,expiring) [after $exptime [mycmd expire_attention_line $cw]]
    debugmsg attline "expiration scheduled for after $exptime"
}

proc attline::cancel_attline_expiration {cw} {
    variable state
    if {[info exists state($cw,expiring)]} {
	after cancel $state($cw,expiring)
	unset state($cw,expiring)
	debugmsg attline "expiration cancelled"
    }
}

proc attline::expire_attention_line {cw} {
    variable state
    variable options

    if {[info exists state($cw,expiring)]} {
	unread $cw false
	unset state($cw,expiring)
	if {$options(remove_expired)} {
	    delete_attention_line $cw
	}
	debugmsg attline "expired"
    }
}

# vim:ts=8:sw=4:sts=4:noet
