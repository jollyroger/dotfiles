# $Id: socials.tcl 615 2005-03-16 22:38:36Z aleksey $

namespace eval socials {
    set social_list {}
}

proc socials::load {filename {enc utf-8}} {
    variable social

    set f [file join [file dirname [info script]] $filename]
    set fd [open $f]
    fconfigure $fd -encoding $enc

    while {1} {
	set s [gets $fd]
	if {$s == "" && [eof $fd]} break
	set s [string trim $s]
	if {$s == "#SOCIALS" || $s == ""} continue
	if {$s == "#0"} break

	set names [split $s]
	set names [lmatch -regexp $names {[^\d]}]
	#puts $names
	add $names

	foreach kind {char_no_arg others_no_arg char_found others_found \
			  vict_found char_not_found char_auto others_auto} {
	    set s [string trim [gets $fd]]
	    if {$s == {$}} {
		continue
	    } elseif {$s == "#"} {
		break
	    }
	    assign $names $kind $s
	}
    }

    close $fd
}


proc socials::add {names} {
    variable social
    variable social_list

    foreach name $names {
	lappend social_list $name
    }
    foreach kind {char_no_arg others_no_arg char_found others_found \
		      vict_found char_not_found char_auto others_auto} {
	assign $names $kind ""
    }
}

proc socials::assign {names kind val} {
    variable social
    foreach name $names {
	set social($kind,$name) $val
    }
}


socials::load socials.ru koi8-r
#puts [array get socials::social]

proc socials::substitute {s from to} {
    regsub -all {\$n\d?} $s /me s
    regsub -all {\$e\d?} $s /me s
    #regsub -all {\$e\d?} $s $from s
    regsub -all {\$N\d?} $s $to s
    regsub -all {\$E\d?} $s $to s
    return $s
}

proc socials::interp {soc from to} {
    variable social

    if {$to == ""} {
	return [substitute $social(others_no_arg,$soc) $from $to]
    } else {
	return [substitute $social(others_found,$soc) $from $to]
    }
}

#puts [socials::interp giggle asd ""]
#puts [socials::interp ползать asd qwe]

proc socials::commands_comps {chatid compsvar wordstart line} {
    variable social_list
    upvar 0 $compsvar comps

    if {!$wordstart} {
	foreach soc $social_list {
	    lappend comps "/$soc "
	}
    }
    #debugmsg completion "SOC: $comps"
}

hook::add generate_completions_hook \
    [namespace current]::socials::commands_comps 51

#puts [llength $socials::social_list]

proc socials::handle_commands {chatid user body type} {
    variable social_list
    variable social

    #if {$type != "groupchat"} return
    if {[string index $body 0] != "/"} return

    set we [string wordend $body 1]
    set command [string trim [crange $body 1 $we]]
    set to [string trim [crange $body $we end]]

    if {[lcontain $social_list $command]} {
	set our_jid [chat::our_jid $chatid]
	set connid [chat::get_connid $chatid]
	if {[catch {chat::get_nick $connid $our_jid groupchat} from]} {
	    set from [chat::get_nick $our_jid groupchat]
	}
	if {$to == $from} {
	    set s [substitute $social(others_auto,$command) $from $to]
	} elseif {$to == ""} {
	    set s [substitute $social(others_no_arg,$command) $from $to]
	    if {$s == ""} {
		set se [substitute $social(char_no_arg,$command) $from $to]
		chat::add_message $chatid $chatid error $se {}
	    }
	} else {
	    set s [substitute $social(others_found,$command) $from $to]
	}

	if {$s != ""} {
	    #hook::run chat_send_message_hook $chatid $user $s $type
	    ::plugins::send_message $chatid $user $s $type
	    ::plugins::draw_message $chatid $user $s $type
	}
	return stop
    }
}

hook::add chat_send_message_hook \
    [namespace current]::socials::handle_commands 50
