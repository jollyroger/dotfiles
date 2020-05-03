# $Id: whiteboard.tcl 1493 2008-08-26 10:32:18Z sergei $

uplevel #0 [list source [file join [file dirname [info script]] svgrender.tcl]]

package require msgcat

::msgcat::mcload [file join [file dirname [info script]] msgs]

namespace eval wb {
    set id_base [pid]	;# used in proc create_id
}

proc wb::add_whiteboard_menu_item {m connid jid} {
    $m add command -label [::msgcat::mc "Whiteboard"] \
	-command [list [namespace current]::open_wb $connid $jid -raise 1]
}

hook::add chat_create_user_menu_hook \
    [namespace current]::wb::add_whiteboard_menu_item 47
hook::add chat_create_conference_menu_hook \
    [namespace current]::wb::add_whiteboard_menu_item 47
hook::add roster_jid_popup_menu_hook \
    [namespace current]::wb::add_whiteboard_menu_item 47

proc wb::open_wb {connid jid args} {
    global dofill

    set raise 0
    foreach {key val} $args {
	switch -- $key {
	    -raise { set raise $val }
	}
    }

    set chatid [chat::chatid $connid $jid]

    set w [win_id whiteboard $chatid]
    if {[winfo exists $w]} {
	if {$raise} {
	    raise_win $w
	}
	return
    }

    set jid [chat::get_jid $chatid]

    set title [format [::msgcat::mc "%s whiteboard"] $jid]
    add_win $w -title $title \
	       -tabtitle $title \
	       -class Whiteboard \
	       -raise $raise


    set sw [ScrolledWindow $w.sw]
    pack $sw -side right -fill both -expand yes
    set c [canvas $w.c -background white]
    $sw setwidget $c
    #pack $c -side right -fill both -expand yes

    bindscroll $c

    set tb [frame $w.tb]
    pack $tb -side left -fill y

    set tbfreehand [radiobutton $w.tb.freehand -text [::msgcat::mc "FreeHand"] \
			-variable [namespace current]::tool($chatid) \
			-value freehand \
			-anchor w \
			-command [list [namespace current]::freehand_bind \
				      $c $chatid]]

    set tbpolyline [radiobutton $w.tb.line -text [::msgcat::mc "PolyLine"] \
			-variable [namespace current]::tool($chatid) \
			-value polyline \
			-anchor w \
			-command [list [namespace current]::line_bind $c $chatid]]
    
    set tbrectangle [radiobutton $w.tb.rectangle -text [::msgcat::mc "Rectangle"] \
			-variable [namespace current]::tool($chatid) \
			-value rectangle \
			-anchor w \
			-command [list [namespace current]::rectangle_bind \
				      $c $chatid]]

    set tbpolygon [radiobutton $w.tb.polygon -text [::msgcat::mc "Polygon"] \
			-variable [namespace current]::tool($chatid) \
			-value polygon \
			-anchor w \
			-command [list [namespace current]::polygon_bind \
				      $c $chatid]]

    set tbcircle [radiobutton $w.tb.circle -text [::msgcat::mc "Circle"] \
			-variable [namespace current]::tool($chatid) \
			-value circle \
			-anchor w \
			-command [list [namespace current]::circle_bind \
				      $c $chatid]]

    set tbtext [radiobutton $w.tb.text -text [::msgcat::mc "Text"] \
			-variable [namespace current]::tool($chatid) \
			-value text \
			-anchor w \
			-command [list [namespace current]::text_bind \
				       $c $chatid]]

    set tbmove [radiobutton $w.tb.move -text [::msgcat::mc "Move/Transform"] \
			-variable [namespace current]::tool($chatid) \
			-value move \
			-anchor w \
			-command [list [namespace current]::move_bind \
				       $c $chatid]]

    pack $tbfreehand $tbpolyline $tbrectangle $tbpolygon $tbcircle $tbtext \
	 $tbmove -anchor w -fill x

    button $w.tb.clear -text [::msgcat::mc "Clear"] \
	-command [list [namespace current]::send_clear $chatid]
    pack $w.tb.clear -side bottom -anchor w -fill x

    button $w.tb.save -text [::msgcat::mc "Save..."] \
	-command [list [namespace current]::save_wb $chatid]
    pack $w.tb.save -side bottom -anchor w -fill x

    #frame $w.tb.spacer1 -relief sunken -bd 1 -height 2 -highlightthickness 0
    #pack $w.tb.spacer1 -side bottom -anchor w -fill x -pady 1m

    set [namespace current]::text_set_fr($chatid) $w.tb
    label $w.tb.example_char -text A
    pack $w.tb.example_char -side bottom -fill x

    button $w.tb.seltextcol -text [::msgcat::mc "Text color"] \
	-command [list [namespace current]::select_text_color \
		      $w.tb.seltextcol $w.tb.example_char]
    pack $w.tb.seltextcol -side bottom -anchor w -fill x

    button $w.tb.selfont -text [::msgcat::mc "Text font"] \
	-command [list [namespace current]::select_font \
		      $w.tb.selfont $w.tb.example_char]
    pack $w.tb.selfont -side bottom -anchor w -fill x

    #frame $w.tb.spacer2 -relief sunken -bd 1 -height 2 -highlightthickness 0
    #pack $w.tb.spacer2 -side bottom -anchor w -fill x -pady 1m

    frame $w.tb.fill

    checkbutton $w.tb.dofill -text [::msgcat::mc "Fill"] -variable dofill
    pack $w.tb.dofill -side left -in $w.tb.fill

    canvas $w.tb.fillcolor -background \#FFFFFF -height 5m -width 5m
    pack $w.tb.fillcolor -side left -padx 3m -in $w.tb.fill

    pack $w.tb.fill -side bottom -anchor w -fill x

    button $w.tb.selfillcol -text [::msgcat::mc "Fill color"] \
	-command [list [namespace current]::select_color \
		      $w.tb.selfillcol $w.tb.fillcolor]
    pack $w.tb.selfillcol -side bottom -anchor w -fill x

    #frame $w.tb.spacer3 -relief sunken -bd 1 -height 2 -highlightthickness 0
    #pack $w.tb.spacer3 -side bottom -anchor w -fill x -pady 1m

    canvas $w.tb.color -background \#000000 -height 0.5c -width 1
    pack $w.tb.color -side bottom

    frame $w.tb.linewidth

    label $w.tb.lwidth -text [::msgcat::mc "Line width: "]
    pack $w.tb.lwidth -side left -in $w.tb.linewidth

    SpinBox $w.tb.width -width 4 -range {1 100} \
	-textvariable [namespace current]::width($chatid)
    pack $w.tb.width -side left -in $w.tb.linewidth

    pack $w.tb.linewidth -side bottom -anchor w -fill x

    button $w.tb.selcol -text [::msgcat::mc "Line color"] \
	-command [list [namespace current]::select_color \
		      $w.tb.selcol $w.tb.color]
    pack $w.tb.selcol -side bottom -anchor w -fill x

    variable balloon
    set balloon($chatid) 1
    checkbutton $w.tb.balloon -text [::msgcat::mc "Show balloons"] \
			      -variable [namespace current]::balloon($chatid)
    pack $w.tb.balloon -side bottom -anchor w -fill x


    $c bind all <Any-Enter>  \
	[list [namespace current]::balloon $chatid $c enter  %X %Y]
    $c bind all <Any-Motion> \
	[list [namespace current]::balloon $chatid $c motion %X %Y]
    $c bind all <Any-Leave>  \
	[list [namespace current]::balloon $chatid $c leave  %X %Y]

    trace variable [namespace current]::width($chatid) w \
	[list [namespace current]::change_width \
	     $w.tb.color [namespace current]::width($chatid)]

    variable tool
    set tool($chatid) move
    move_bind $c $chatid
}

proc wb::balloon {chatid c action X Y} {
    variable balloon

    if {!$balloon($chatid)} return

    set id [$c find withtag current]
    set tags {}
    foreach t [$c gettags $id] {
	if {[lindex $t 0] == "time"} {
	    lappend tags $t
	}
    }

    set msgs {}
    foreach t [lsort -index 1 -integer $tags] {
	lappend msgs [lindex $t 2]
    }

    balloon::default_balloon $c:$id $action $X $Y -text [join $msgs "\n"]
}

proc wb::select_color {but col} {
    set color [SelectColor::menu $col.color [list below $but] \
		   -color [$col cget -background]]

    if {[string length $color]} {
        $col configure -background $color
    }
}

proc wb::select_font {chatid col} {
    variable app_font
    set font_desc [SelectFont .s -type dialog]

    if {[string length $font_desc] == 0} return

    if {![info exists app_font($font_desc)]} {
        set app_font($font_desc) \
	    [eval font create [list $font_desc] [font actual $font_desc]]
    }
    $col configure -font $app_font($font_desc)
}

proc wb::select_text_color {but col} {
    set color [SelectColor::menu $col.color [list below $but] \
		   -color [$col cget -foreground]]

    if {[string length $color]} {
        $col configure -foreground $color
    }
}

proc wb::get_text_color {chatid} {
    [set [namespace current]::text_set_fr($chatid)].example_char cget -foreground
}

proc wb::get_text_font {chatid} {
    [set [namespace current]::text_set_fr($chatid)].example_char cget -font
}

proc wb::get_fill_color {chatid} {
    set w [win_id whiteboard $chatid]
    $w.tb.fillcolor cget -background
}

proc wb::get_color {chatid} {
    set w [win_id whiteboard $chatid]
    $w.tb.color cget -background
}

proc wb::change_width {col widthvar args} {
    set width [set $widthvar]
    $col configure -width $width
}

proc wb::get_width {chatid} {
    set w [win_id whiteboard $chatid]
    $w.tb.color cget -width
}

proc wb::save_wb {chatid} {
    set w [win_id whiteboard $chatid]

    set filepath [tk_getSaveFile -defaultextension .eps \
				 -filetypes {{{Encapsulated PostScript files} *.eps}
					     {{All files}        *}}]

    if {$filepath == ""} return
    $w.c postscript -file $filepath
}

###############################################################################

proc wb::create_id {} {
    # Unseeded random number is not good enough, because remote clients
    # are likely to produce the same numbers.
    return [rand 10000][clock seconds][set [namespace current]::id_base]
}

proc wb::send_svg {chatid tag} {
    if {[chat::is_groupchat $chatid]} {
	set type groupchat
    } else {
	set type chat
    }
    set connid [chat::get_connid $chatid]
    set jid [chat::get_jid $chatid]
    jlib::send_msg $jid \
	-connection $connid \
	-type $type \
	-xlist [list [jlib::wrapper:createtag x \
			  -vars {xmlns tkabber:whiteboard} \
			  -subtags [list [jlib::wrapper:createtag svg \
					      -subtags [list $tag]]]]]
}

proc wb::send_clear {chatid} {
    if {[chat::is_groupchat $chatid]} {
	set type groupchat
    } else {
	set type chat
        [win_id whiteboard $chatid].c delete all
    }
    set connid [chat::get_connid $chatid]
    set jid [chat::get_jid $chatid]
    jlib::send_msg $jid -type $type \
	-connection $connid \
	-xlist [list [jlib::wrapper:createtag x \
			  -vars {xmlns tkabber:whiteboard} \
			  -subtags [list [jlib::wrapper:createtag clear]]]]
}

proc wb::handle_wb {chatid from type body x} {
    set seconds [jlib::x_delay $x]

    foreach xelem $x {
	jlib::wrapper:splitxml $xelem tag vars isempty chdata children

	if {[string equal [jlib::wrapper:getattr $vars xmlns] \
			  tkabber:whiteboard]} {
	    open_wb [chat::get_connid $chatid] [chat::get_jid $chatid]
	    set w [win_id whiteboard $chatid]
	    foreach child $children {
		parse_item $chatid $from $seconds $child
	    }
	    tab_set_updated $w 1 message

	    $w.c configure -scrollregion [$w.c bbox all]
	}
    }
}
hook::add draw_message_hook [namespace current]::wb::handle_wb 1

proc wb::parse_item {chatid from seconds item} {
    set w [win_id whiteboard $chatid]
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    switch -- $tag {
	svg {
	    foreach child $children {
		set id [svg::parseSVGItem $w.c {} {} $child]
		if {$id != ""} {
		    $w.c addtag [list tag $child] withtag $id
		    $w.c addtag [time_tag created $from $seconds] withtag $id
		}
	    }
	}
	transform {
	    set id [jlib::wrapper:getattr $vars id]
	    set transform [jlib::wrapper:getattr $vars transform]
	    set transform1 $transform
	    set tags [$w.c gettags [list id $id]]
	    set child {}
	    foreach t $tags {
		if {[lindex $t 0] == "tag"} {
		    set child [lindex $t 1]
		    break
		}
	    }
	    foreach t $tags {
		if {[lindex $t 0] == "transform"} {
		    set transform "$transform [lindex $t 1]"
		    break
		}
	    }
	    if {$child != {}} {
		$w.c delete [list id $id]
		set id1 [svg::parseSVGItem $w.c [svg::ParseTransform $transform] {} $child]
		if {$id1 != ""} {
		    foreach t $tags {
			$w.c addtag $t withtag $id1
		    }
		    add_transform_tag $w.c $id $transform1
		    $w.c addtag [time_tag transformed $from $seconds] withtag $id1
		}
	    }
	}
	move {
	    set id [jlib::wrapper:getattr $vars id]
	    set dx [jlib::wrapper:getattr $vars dx]
	    set dy [jlib::wrapper:getattr $vars dy]
	    if {![string is double $dx] || $dx == ""} {set dx 0}
	    if {![string is double $dy] || $dy == ""} {set dy 0}
	    add_transform_tag $w.c $id translate($dx,$dy)
	    $w.c addtag [time_tag moved $from $seconds] withtag [list id $id]
	    $w.c move [list id $id] $dx $dy
	}
	remove {
	    $w.c delete [list id [jlib::wrapper:getattr $vars id]]
	}
	clear {
	    $w.c delete all
	}
    }
}

###############################################################################

proc wb::add_transform_tag {c id transform} {
    set tags [$c gettags [list id $id]]
    foreach t $tags {
	if {[lindex $t 0] == "transform"} {
	    set transform "$transform [lindex $t 1]"
	    $c dtag [list id $id] $t
	    break
	}
    }
    $c addtag [list transform $transform] withtag [list id $id]
}

proc wb::time_tag {type jid {seconds ""}} {
    set seconds_now [clock seconds]
    set format $::plugins::options(timestamp_format)

    set seconds_day_before [clock scan "-23 hours 59 minutes" -base $seconds_now]
    if {$seconds == ""} {
	set seconds $seconds_now
    }
    if {$seconds <= $seconds_day_before} {
	set format $::plugins::options(delayed_timestamp_format)
    }
    set time [clock format $seconds -format $format]
    switch -- $type {
	created {
	    return [list time $seconds [::msgcat::mc "%s created: %s" $time $jid]]
	}
	moved {
	    return [list time $seconds [::msgcat::mc "%s moved: %s" $time $jid]]
	}
	transformed {
	    return [list time $seconds [::msgcat::mc "%s transformed: %s" $time $jid]]
	}
	default {
	    return ""
	}
    }
}

###############################################################################

proc wb::popup_menu {c chatid x y} {
    set m .whiteboard_popup_menu

    if {[winfo exists $m]} {
	destroy $m
    }

    set tags [$c gettags current]
    set id ""
    foreach t $tags {
	if {[lindex $t 0] == "id"} {
	    set id [lindex $t 1]
	    break
	}
    }

    menu $m -tearoff 0

    if {![string equal $id ""]} {
	set state normal
    } else {
	set state disabled
    }

    $m add command -label [::msgcat::mc "Flip horizontally"] \
		   -command [list [namespace current]::flip_h $c $chatid $id] \
		   -state $state
    $m add command -label [::msgcat::mc "Flip vertically"] \
		   -command [list [namespace current]::flip_v $c $chatid $id] \
		   -state $state
    $m add command -label [::msgcat::mc "Rotate 45\u00b0"] \
		   -command [list [namespace current]::rotate $c $chatid $id -45] \
		   -state $state
    $m add command -label [::msgcat::mc "Rotate 90\u00b0"] \
		   -command [list [namespace current]::rotate $c $chatid $id -90] \
		   -state $state
    $m add command -label [::msgcat::mc "Rotate 135\u00b0"] \
		   -command [list [namespace current]::rotate $c $chatid $id -135] \
		   -state $state
    $m add command -label [::msgcat::mc "Rotate 180\u00b0"] \
		   -command [list [namespace current]::rotate $c $chatid $id -180] \
		   -state $state

    $m add separator

    $m add command -label [::msgcat::mc "Remove"] \
		   -command [list [namespace current]::remove_b1p $c $chatid $id] \
		   -state $state

    tk_popup $m $x $y
}

###############################################################################

proc wb::flip_h {c chatid id} {
    lassign [$c bbox [list id $id]] x1 y1 x2 y2
    set x02 [expr {$x1+$x2}]
    transform $c $chatid $id "translate($x02,0) scale(-1,1)"
}

proc wb::flip_v {c chatid id} {
    lassign [$c bbox [list id $id]] x1 y1 x2 y2
    set y02 [expr {$y1+$y2}]
    transform $c $chatid $id "translate(0,$y02) scale(1,-1)"
}

proc wb::rotate {c chatid id angle} {
    lassign [$c bbox [list id $id]] x1 y1 x2 y2
    set x0 [expr {($x1+$x2)/2}]
    set y0 [expr {($y1+$y2)/2}]
    transform $c $chatid $id "rotate($angle,$x0,$y0)"
}

proc wb::transform {c chatid id transform} {
    if {[chat::is_groupchat $chatid]} {
	set type groupchat
    } else {
	set type chat
	set jid [jlib::connection_jid [chat::get_connid $chatid]]
	add_transform_tag $c $id $transform
	$c addtag [time_tag transformed $jid] withtag [list id $id]
    }

    set vars [list id $id transform $transform]

    set connid [chat::get_connid $chatid]
    set jid [chat::get_jid $chatid]
    jlib::send_msg $jid \
	-connection $connid \
	-type $type \
	-xlist [list [jlib::wrapper:createtag x \
			  -vars {xmlns tkabber:whiteboard} \
			  -subtags [list [jlib::wrapper:createtag transform \
					      -vars $vars]]]]
}

###############################################################################

###############################################################################
# Line

proc wb::line_bind {c chatid} {
    bind $c <ButtonPress-1> \
	[list [namespace current]::line_b1 [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> {}
    bind $c <Motion> [list [namespace current]::line_b1m [double% $c] %x %y]
    bind $c <ButtonRelease-1> {}
    bind $c <Button-3> [list [namespace current]::line_b3 \
			    [double% $c] [double% $chatid]]
    $c configure -cursor crosshair
}

proc wb::line_b1 {c chatid x y} {
    variable line

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    if {[info exists line(drawed)]} {
	lappend line(coords) $x $y
	catch {$c delete $line(temp)}
	set tag [line_tag ""]
        set line(temp) [svg::parseSVGItem $c {} {} $tag]
    } else {
	set line(drawed) 1
	set line(coords) [list $x $y]
	set line(options) [list stroke-linejoin miter stroke [get_color $chatid]]
	if {[set width [get_width $chatid]] != 1} {
	    lappend line(options) stroke-width $width
	}

    }
}

proc wb::line_b1m {c x y} {
    variable line

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    if {[info exists line(drawed)]} {
	set coords $line(coords)
	lappend line(coords) $x $y
	catch {$c delete $line(temp)}
	set tag [line_tag ""]
        set line(temp) [svg::parseSVGItem $c {} {} $tag]
	set line(coords) $coords
    }
}


proc wb::line_b3 {c chatid} {
    variable line

    catch {
	unset line(drawed)

	set id [create_id]
	catch {$c delete $line(temp)}
	set tag [line_tag $id]
        set line(temp) [svg::parseSVGItem $c {} {} $tag]

	if {[chat::is_groupchat $chatid]} {
	    $c delete $line(temp)
	} else {
	    set jid [jlib::connection_jid [chat::get_connid $chatid]]
	    $c addtag [list tag $tag] withtag $line(temp)
	    $c addtag [time_tag created $jid] withtag $line(temp)
	}

	send_svg $chatid $tag

	set line(coords) {}
	set line(temp) {}
    }
}

proc wb::line_tag {id} {
    variable line

    set vars $line(options)
    lappend vars points $line(coords)
    if {$id != ""} {
	lappend vars id $id
    }
    return [jlib::wrapper:createtag polyline -vars $vars]
}

###############################################################################

###############################################################################
# Polygon

proc wb::polygon_bind {c chatid} {
    bind $c <ButtonPress-1> \
	[list [namespace current]::polygon_b1 [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> {}
    bind $c <Motion> [list [namespace current]::polygon_m [double% $c] %x %y]
    bind $c <ButtonRelease-1> {}
    bind $c <Button-3> [list [namespace current]::polygon_b3 \
			    [double% $c] [double% $chatid]]
    $c configure -cursor crosshair
}

proc wb::polygon_b1 {c chatid x y} {
    variable polygon
    variable line1
    variable line2
    global dofill

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    if {[info exists polygon(drawed)]} {
	lappend polygon(coords) $x $y

	catch {$c delete $line1(temp)}
	catch {$c delete $line2(temp)}
	catch {$c delete $polygon(temp)}
	set tag [polygon_tag ""]
        set polygon(temp) [svg::parseSVGItem $c {} {} $tag]
    } else {
	set polygon(drawed) 1
	set polygon(coords) [list $x $y]
	set polygon(line_options) [list -fill [get_color $chatid] \
			       -width [get_width $chatid] \
			       -joinstyle miter]
	set polygon(options) [list stroke-linejoin miter stroke [get_color $chatid]]
	if {[set width [get_width $chatid]] != 1} {
	    lappend polygon(options) stroke-width $width
	}
	if {$dofill == 1} {
	    lappend polygon(options) fill [get_fill_color $chatid]
	}
    }
}

proc wb::polygon_m {c x y} {
    variable polygon
    variable line1
    variable line2

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    if {[info exists polygon(drawed)]} {
	set x1 [lindex $polygon(coords) 0]
	set y1 [lindex $polygon(coords) 1]
	set xn [lindex $polygon(coords) end-1]
	set yn [lindex $polygon(coords) end]

	catch {$c delete $line1(temp)}
	catch {$c delete $line2(temp)}
	set line1(temp) [eval $c create line $x1 $y1 $x $y $polygon(line_options)]
	set line2(temp) [eval $c create line $xn $yn $x $y $polygon(line_options)]
    }
}


proc wb::polygon_b3 {c chatid} {
    variable polygon
    variable line1
    variable line2
    global dofill

    catch {
	unset polygon(drawed)

	set id [create_id]
	catch {$c delete $line1(temp)}
	catch {$c delete $line2(temp)}
	catch {$c delete $polygon(temp)}

	set tag [polygon_tag $id]

	if {[llength $polygon(coords)] > 4} {
	    set polygon(temp) [svg::parseSVGItem $c {} {} $tag]

	    if {[chat::is_groupchat $chatid]} {
		$c delete $polygon(temp)
	    } else {
		set jid [jlib::connection_jid [chat::get_connid $chatid]]
		$c addtag [list tag $tag] withtag $polygon(temp)
		$c addtag [time_tag created $jid] withtag $polygon(temp)
	    }

	    send_svg $chatid $tag
	}
	set polygon(coords) {}
	set polygon(temp) {}
    }
}

proc wb::polygon_tag {id} {
    variable polygon

    set vars $polygon(options)
    lappend vars points $polygon(coords)
    if {$id != ""} {
	lappend vars id $id
    }
    return [jlib::wrapper:createtag polygon -vars $vars]
}

###############################################################################

###############################################################################
# Rectangle

proc wb::rectangle_bind {c chatid} {
    bind $c <ButtonPress-1> \
	[list [namespace current]::rectangle_b1 [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> \
 	[list [namespace current]::rectangle_b1m [double% $c] %x %y]
    bind $c <Motion> {}
    bind $c <ButtonRelease-1> \
	[list [namespace current]::rectangle_b1r [double% $c] [double% $chatid] %x %y]
    bind $c <Button-3> {}
    $c configure -cursor crosshair
}

proc wb::rectangle_b1 {c chatid x y} {
    variable rectangle
    global dofill

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    set rectangle(drawed) 1
    set rectangle(x1) $x
    set rectangle(y1) $y
    set rectangle(options) [list stroke [get_color $chatid]]
    if {$dofill == 1} {
	lappend rectangle(options) fill [get_fill_color $chatid]
    }
    if {[set width [get_width $chatid]] != 1} {
	lappend rectangle(options) stroke-width $width
    }
}

proc wb::rectangle_b1r {c chatid x y} {
    variable rectangle

    if {[info exists rectangle(drawed)]} {
        unset rectangle(drawed)
	set rectangle(x2) [$c canvasx $x]
	set rectangle(y2) [$c canvasy $y]

        set id [create_id]
	set tag [rectangle_tag $id]

        catch {$c delete $rectangle(temp)}
        set rectangle(temp) [svg::parseSVGItem $c {} {} $tag]

	if {[chat::is_groupchat $chatid]} {
	    $c delete $rectangle(temp)
	} else {
	    set jid [jlib::connection_jid [chat::get_connid $chatid]]
	    $c addtag [list tag $tag] withtag $rectangle(temp)
	    $c addtag [time_tag created $jid] withtag $rectangle(temp)
	}

	send_svg $chatid $tag

        unset rectangle(x1)
        unset rectangle(x2)
        unset rectangle(y1)
        unset rectangle(y2)
        set rectangle(temp) {}
    }
}

proc wb::rectangle_b1m {c x y} {
    variable rectangle

    if {[info exists rectangle(drawed)]} {
	set rectangle(x2) [$c canvasx $x]
	set rectangle(y2) [$c canvasy $y]

	set tag [rectangle_tag ""]

	catch {$c delete $rectangle(temp)}
        set rectangle(temp) [svg::parseSVGItem $c {} {} $tag]
    }
}

proc wb::rectangle_tag {id} {
    variable rectangle

    set vars $rectangle(options)
    if {$rectangle(x2) > $rectangle(x1)} {
	lappend vars x $rectangle(x1) \
		     width [expr {$rectangle(x2) - $rectangle(x1)}]
    } else {
	lappend vars x $rectangle(x2) \
		     width [expr {$rectangle(x1) - $rectangle(x2)}]
    }
    if {$rectangle(y2) > $rectangle(y1)} {
	lappend vars y $rectangle(y1) \
		     height [expr {$rectangle(y2) - $rectangle(y1)}]
    } else {
	lappend vars y $rectangle(y2) \
		     height [expr {$rectangle(y1) - $rectangle(y2)}]
    }
    if {$id != ""} {
	lappend vars id $id
    }
    return [jlib::wrapper:createtag rect -vars $vars]
}

###############################################################################

###############################################################################
# Circle

proc wb::circle_bind {c chatid} {
    bind $c <ButtonPress-1> \
	[list [namespace current]::circle_b1 [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> \
 	[list [namespace current]::circle_b1m [double% $c] %x %y]
    bind $c <Motion> {}
    bind $c <ButtonRelease-1> \
	[list [namespace current]::circle_b1r [double% $c] [double% $chatid] %x %y]
    bind $c <Button-3> {}
    $c configure -cursor crosshair
}

proc wb::circle_b1 {c chatid x y} {
    variable circle
    global dofill

    set cx [$c canvasx $x]
    set cy [$c canvasy $y]

    set circle(drawed) 1
    set circle(cx) $cx
    set circle(cy) $cy
    set circle(options) [list cx $cx cy $cy stroke [get_color $chatid]]
    if {$dofill == 1} {
	lappend circle(options) fill [get_fill_color $chatid]
    }
    if {[set width [get_width $chatid]] != 1} {
	lappend circle(options) stroke-width $width
    }
}

proc wb::circle_b1r {c chatid x y} {
    variable circle

    if {[info exists circle(drawed)]} {
        unset circle(drawed)
	set cx $circle(cx)
	set cy $circle(cy)
	set x [$c canvasx $x]
	set y [$c canvasy $y]
	set r [expr {hypot($cx - $x, $cy - $y)}]

        set id [create_id]
	set tag [circle_tag $id $r]

        catch {$c delete $circle(temp)}
        set circle(temp) [svg::parseSVGItem $c {} {} $tag]

	if {[chat::is_groupchat $chatid]} {
	    $c delete $circle(temp)
	} else {
	    set jid [jlib::connection_jid [chat::get_connid $chatid]]
	    $c addtag [list tag $tag] withtag $circle(temp)
	    $c addtag [time_tag created $jid] withtag $circle(temp)
	}

	send_svg $chatid $tag

        unset circle(cx)
        unset circle(cy)
        set circle(temp) {}
    }
}

proc wb::circle_b1m {c x y} {
    variable circle

    if {[info exists circle(drawed)]} {
	set cx $circle(cx)
	set cy $circle(cy)
	set x [$c canvasx $x]
	set y [$c canvasy $y]
	set r [expr hypot($cx - $x, $cy - $y)]

	set tag [circle_tag "" $r]

	catch {$c delete $circle(temp)}
        set circle(temp) [svg::parseSVGItem $c {} {} $tag]
    }
}

proc wb::circle_tag {id r} {
    variable circle

    set vars $circle(options)
    lappend vars r $r
    if {$id != ""} {
	lappend vars id $id
    }
    return [jlib::wrapper:createtag circle -vars $vars]
}

###############################################################################

###############################################################################
# Freehand

proc wb::freehand_bind {c chatid} {
    bind $c <ButtonPress-1> \
	[list [namespace current]::freehand_b1p \
	     [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> [list [namespace current]::freehand_b1m \
			     [double% $c] %x %y]
    bind $c <ButtonRelease-1> \
	[list [namespace current]::freehand_b1r [double% $c] [double% $chatid]]
    bind $c <Button-3> {}
    $c configure -cursor crosshair
}

proc wb::freehand_b1p {c chatid x y} {
    variable line

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    set line(drawed) 1
    set line(coords) [list $x $y]
    set line(options) [list stroke-linejoin round stroke [get_color $chatid]]
    if {[set width [get_width $chatid]] != 1} {
	lappend line(options) stroke-width $width
    }
}

proc wb::freehand_b1m {c x y} {
    variable line

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    if {[info exists line(drawed)]} {
	lappend line(coords) $x $y

	catch {$c delete $line(temp)}
	set tag [freehand_tag ""]
	set line(temp) [svg::parseSVGItem $c {} {} $tag]
    }
}


proc wb::freehand_b1r {c chatid} {
    variable line

    catch {
	unset line(drawed)

	set id [create_id]

	set tag [freehand_tag $id]

	catch {$c delete $line(temp)}
	set line(temp) [svg::parseSVGItem $c {} {} $tag]

	if {[chat::is_groupchat $chatid]} {
	    $c delete $line(temp)
	} else {
	    set jid [jlib::connection_jid [chat::get_connid $chatid]]
	    $c addtag [list tag $tag] withtag $line(temp)
	    $c addtag [time_tag created $jid] withtag $line(temp)
	}

	send_svg $chatid $tag

	set line(coords) {}
	set line(temp) {}
    }
}

proc wb::freehand_tag {id} {
    variable line

    set vars $line(options)
    lappend vars points $line(coords)
    if {$id != ""} {
	lappend vars id $id
    }
    return [jlib::wrapper:createtag polyline -vars $vars]
}

###############################################################################

###############################################################################
# Remove

proc wb::remove_b1p {c chatid id} {
    set connid [chat::get_connid $chatid]
    set jid [chat::get_jid $chatid]
    if {[chat::is_groupchat $chatid]} {
	set type groupchat
    } else {
	set type chat
	$c delete [list id $id]
    }
    if {$id != ""} {
	jlib::send_msg $jid \
	    -connection $connid \
	    -type $type \
	    -xlist [list [jlib::wrapper:createtag x \
			      -vars {xmlns tkabber:whiteboard} \
			      -subtags [list [jlib::wrapper:createtag remove \
						  -vars [list id $id]]]]]
    }
}


###############################################################################

###############################################################################
# Move

proc wb::move_bind {c chatid} {
    bind $c <ButtonPress-1> \
	 [list [namespace current]::move_b1p [double% $c] [double% $chatid] %x %y]
    bind $c <B1-Motion> \
	 [list [namespace current]::move_b1m [double% $c] %x %y]
    bind $c <ButtonRelease-1> \
	 [list [namespace current]::move_b1r [double% $c] [double% $chatid]]
    bind $c <Button-3> \
	 [list [namespace current]::popup_menu [double% $c] [double% $chatid] %X %Y]
    bind $c <Motion> {}
    $c configure -cursor ""
}

proc wb::move_b1p {c chatid x y} {
    variable move

    set tags [$c gettags current]
    set id ""
    foreach t $tags {
	if {[lindex $t 0] == "id"} {
	    set id [lindex $t 1]
	    break
	}
    }
    if {$id != ""} {
	set x [$c canvasx $x]
	set y [$c canvasy $y]

	set move(startx) $x
	set move(starty) $y
        set move(lastx) $x
	set move(lasty) $y

	set move(id) $id
	$c configure -cursor hand2
    } else {
	catch {unset move(id)}
    }
}

proc wb::move_b1m {c x y} {
    variable move

    if {[info exists move(id)]} {
	set id $move(id)
	set x [$c canvasx $x]
	set y [$c canvasy $y]

	$c move [list id $id] [expr {$x - $move(lastx)}] [expr {$y - $move(lasty)}]

	set move(lastx) $x
	set move(lasty) $y
    }
}


proc wb::move_b1r {c chatid} {
    variable move

    if {[info exists move(id)]} {
	set id $move(id)
	set x $move(lastx)
	set y $move(lasty)

	set dx [expr {$x - $move(startx)}]
	set dy [expr {$y - $move(starty)}]

	if {$dx == 0 && $dy == 0} {
	    $c configure -cursor ""
	    return
	}

	if {[chat::is_groupchat $chatid]} {
	    set type groupchat
	    $c move [list id $id] [expr {-$dx}] [expr {-$dy}]
	} else {
	    set type chat
	    set jid [jlib::connection_jid [chat::get_connid $chatid]]
	    $c addtag [time_tag moved $jid] withtag [list id $id]
	}

	set vars [list id $id dx $dx dy $dy]

	set connid [chat::get_connid $chatid]
	set jid [chat::get_jid $chatid]
	jlib::send_msg $jid \
	    -connection $connid \
	    -type $type \
	    -xlist [list [jlib::wrapper:createtag x \
			      -vars {xmlns tkabber:whiteboard} \
			      -subtags [list [jlib::wrapper:createtag move \
						  -vars $vars]]]]
	$c configure -cursor ""
    }
}

###############################################################################
# Text

proc wb::text_bind {c chatid} {
    bind $c <ButtonPress-1> {}
    bind $c <B1-Motion> {}
    bind $c <ButtonRelease-1> \
	    [list [namespace current]::text_b1 [double% $c] [double% $chatid] %x %y]
    bind $c <Button-3> {}
    $c configure -cursor crosshair
}

proc wb::text_b1 {c chatid x y} {
    variable text_info
    set text_info(x) [$c canvasx $x]
    set text_info(y) [$c canvasy $y]
    set w [win_id whiteboard $chatid]
    set wt $w.text_dialog
    if {[winfo exists $wt]} {
	wm deiconify $wt
    } else {
	Dialog $wt -anchor e \
		   -separator yes \
		   -title [::msgcat::mc "Enter text"] \
		   -side bottom \
		   -modal none \
		   -default 0 \
		   -cancel 1
	$wt add -text [::msgcat::mc "OK"] \
	    -command [list [namespace current]::text_ok $wt $c $chatid]
	$wt add -text [::msgcat::mc "Cancel"] \
	    -command [list [namespace current]::text_cancel $wt $c $chatid]

	set en [entry $wt.text -width 80 \
		    -textvariable [namespace current]::text_entered($chatid)]
	pack $en -side top -in [$wt getframe]
	$wt draw $en
    }
}

proc wb::text_ok {wt c chatid} {
    variable app_font
    variable text_info
    set id [create_id]

    set text [set [namespace current]::text_entered($chatid)]

    set vars [list id $id x $text_info(x) y $text_info(y) \
		   fill [get_text_color $chatid]]
    set font [get_text_font $chatid]
    if {[info exists app_font($font)]} {
	array set font_opt [font configure $font]
	lappend vars font-size $font_opt(-size) \
		     font-family $font_opt(-family)
	if {$font_opt(-underline) || $font_opt(-overstrike)} {
	    set dec {}
	    if {$font_opt(-underline)} {
		lappend dec underline
	    }
	    if {$font_opt(-overstrike)} {
		lappend dec line-through
	    }
	    lappend vars text-decoration $dec
	}
	if {[string equal $font_opt(-slant) italic]} {
	    lappend vars font-style italic
	}
	if {[string equal $font_opt(-weight) bold]} {
	    lappend vars font-weight bold
	}
	unset font_opt
    }

    set tag [jlib::wrapper:createtag text -vars $vars -chdata $text]

    if {![chat::is_groupchat $chatid]} {
	set textid [svg::parseSVGItem $c {} {} $tag]
	set jid [jlib::connection_jid [chat::get_connid $chatid]]
	$c addtag [list tag $tag] withtag $textid
	$c addtag [time_tag created $jid] withtag $textid
    }

    send_svg $chatid $tag
    wm withdraw $wt
}

proc wb::text_cancel {wt c chatid} {
    wm withdraw $wt
}

###############################################################################

