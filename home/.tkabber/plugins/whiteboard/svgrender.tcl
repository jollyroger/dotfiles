# $Id: svgrender.tcl 1471 2008-07-08 18:46:04Z sergei $

namespace eval svg {
    # Smoothed lines and polygons are available in Tk starting from 8.5
    if {[package vsatisfies [package provide Tk] 8.5]} {
	variable Smooth 1
    } else {
	variable Smooth 0
    }

    variable Debug 0
}

proc svg::loadFile {c filename} {
    set f [open $filename]
    set file [read $f]
    close $f

    set parser [jlib::wrapper:new "#" "#" \
				  [list [namespace current]::parseSVG $c]]
    jlib::wrapper:elementstart $parser stream:stream {} {}
    jlib::wrapper:parser $parser parse $file
    jlib::wrapper:parser $parser configure -final 0
}

proc svg::parseSVG {c xmldata} {
    Debug 2 $xmldata
    jlib::wrapper:splitxml $xmldata tag vars isempty chdata children

    if {$tag != "svg"} {
	return -code error "Not a SVG file"
    }

    parseSVGItem $c {} {} $xmldata
}

proc svg::parseSVGItem {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    switch -- $tag {
	svg {
	    ParseSVG $c $transform $curAttrs $item
	}
	rect {
	    ParseRect $c $transform $curAttrs $item
	}
	line {
	    ParseLine $c $transform $curAttrs $item
	}
	polyline {
	    ParsePolyline $c $transform $curAttrs $item
	}
	polygon {
	    ParsePolygon $c $transform $curAttrs $item
	}
	circle {
	    ParseCircle $c $transform $curAttrs $item
	}
	ellipse {
	    ParseEllipse $c $transform $curAttrs $item
	}
	text {
	    ParseText $c $transform $curAttrs $item
	}
	g {
	    ParseG $c $transform $curAttrs $vars $children
	}
	default {
	    Debug 1 Unknown SVG tag '$tag'
	    return ""
	}
    }
}

proc svg::ParseSVG {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    # TODO

    foreach child $children {
	parseSVGItem $c $transform $curAttrs $child
    }
    return ""
}

proc svg::ParseG {c transform curAttrs vars items} {
    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    array set attrs $curAttrs
    array set attrs $vars
    set curAttrs [array get attrs]

    foreach item $items {
	parseSVGItem $c $transform $curAttrs $item
    }
    return ""
}

proc svg::ParsePolygon {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    set p [TransformPoints $transform [jlib::wrapper:getattr $vars points]]

    array set attrs $curAttrs
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem line

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    fill -
	    stroke -
	    stroke-width -
	    stroke-linejoin {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    if {[info exists attrs(fill)]} {
	set opts [PolygonOpts]
	set drawitem polygon
    } else {
	set opts [LineOpts]
	set drawitem line
	lappend p [lindex $p 0] [lindex $p 1]
    }

    Debug 2 $drawitem $p $opts
    eval [list $c create $drawitem] $p $opts
}

proc svg::ParseCircle {c transform curAttrs item} {
    variable Smooth
    variable Unitcircle

    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    array set attrs $curAttrs
    array set attrs {cx 0 cy 0 r 0}
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem circle

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    cx -
	    cy -
	    r -
	    fill -
	    stroke -
	    stroke-width {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    set opts [CircleOpts]

    if {$Smooth} {
	set points {}
	for {set i 0} {$i < 30} {incr i} {
	    set a [expr {3.1415926 * $i / 15}]
	    lappend points [expr {$attrs(cx) + $attrs(r)*cos($a)}] \
		           [expr {$attrs(cy) + $attrs(r)*sin($a)}]
	}
    } else {
	set points {}
	for {set i 0} {$i < 180} {incr i} {
	    set a [expr {3.1415926 * $i / 90}]
	    lappend points [expr {$attrs(cx) + $attrs(r)*cos($a)}] \
		           [expr {$attrs(cy) + $attrs(r)*sin($a)}]
	}
    }

    set points [TransformPoints $transform $points]

    Debug 2 polygon $points $opts
    eval [list $c create polygon $points] $opts
}

proc svg::ParseEllipse {c transform curAttrs item} {
    variable Smooth
    variable Unitcircle

    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    array set attrs $curAttrs
    array set attrs {cx 0 cy 0 rx 0 ry 0}
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem circle

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    cx -
	    cy -
	    rx -
	    ry -
	    fill -
	    stroke -
	    stroke-width {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    set opts [CircleOpts]

    if {$Smooth} {
	set points {}
	for {set i 0} {$i < 30} {incr i} {
	    set a [expr {3.1415926 * $i / 15}]
	    lappend points [expr {$attrs(cx) + $attrs(rx)*cos($a)}] \
		           [expr {$attrs(cy) + $attrs(ry)*sin($a)}]
	}
    } else {
	set points {}
	for {set i 0} {$i < 180} {incr i} {
	    set a [expr {3.1415926 * $i / 90}]
	    lappend points [expr {$attrs(cx) + $attrs(rx)*cos($a)}] \
		           [expr {$attrs(cy) + $attrs(ry)*sin($a)}]
	}
    }

    set points [TransformPoints $transform $points]

    Debug 2 polygon $points $opts
    eval [list $c create polygon $points] $opts
}

proc svg::ParseLine {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    set x1 [jlib::wrapper:getattr $vars x1]
    set y1 [jlib::wrapper:getattr $vars y1]
    set x2 [jlib::wrapper:getattr $vars x2]
    set y2 [jlib::wrapper:getattr $vars y2]

    foreach {x1 y1} [TransformCoord $transform $x1 $y1] break
    foreach {x2 y2} [TransformCoord $transform $x2 $y2] break

    array set attrs $curAttrs
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem line

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    stroke -
	    stroke-width -
	    stroke-linecap -
	    stroke-linejoin {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    set opts [LineOpts]

    Debug 2 line $x1 $y1 $x2 $y2 $opts
    eval [list $c create line $x1 $y1 $x2 $y2] $opts
}

proc svg::ParseRect {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    set x      [jlib::wrapper:getattr $vars x]
    set y      [jlib::wrapper:getattr $vars y]
    set width  [jlib::wrapper:getattr $vars width]
    set height [jlib::wrapper:getattr $vars height]
    set rx     [jlib::wrapper:getattr $vars rx]
    set ry     [jlib::wrapper:getattr $vars ry]
    if {$rx != ""} {
	Debug 1 Round corners are ignored
    }
    set x2 [expr {$x + $width}]
    set y2 [expr {$y + $height}]

    foreach {xx yy} [TransformCoord $transform $x $y] break
    foreach {xx1 yy1} [TransformCoord $transform $x2 $y] break
    foreach {xx2 yy2} [TransformCoord $transform $x2 $y2] break
    foreach {xx3 yy3} [TransformCoord $transform $x $y2] break

    array set attrs $curAttrs
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem line

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    stroke -
	    stroke-width -
	    stroke-linejoin {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    set opts [PolygonOpts]

    Debug 2 polygon $xx $yy $xx1 $yy1 $xx2 $yy2 $xx3 $yy3] $opts
    eval [list $c create polygon $xx $yy $xx1 $yy1 $xx2 $yy2 $xx3 $yy3] $opts
}

proc svg::ParsePolyline {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    set p [TransformPoints $transform [jlib::wrapper:getattr $vars points]]

    array set attrs $curAttrs
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]
    set drawitem line

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	switch -- $attr {
	    "" {}
	    fill -
	    stroke -
	    stroke-width -
	    stroke-linecap -
	    stroke-linejoin {
		set attrs($attr) $val
	    }
	    default {
		Debug 1 Unknown style attr '$attr'
	    }
	}
    }

    set opts [LineOpts]

    Debug 2 line $p $opts
    eval [list $c create line] $p $opts
}

proc svg::ParseText {c transform curAttrs item} {
    jlib::wrapper:splitxml $item tag vars isempty chdata children

    eval lappend transform \
	 [ParseTransform [jlib::wrapper:getattr $vars transform]]

    set x  [jlib::wrapper:getattr $vars x]
    set y  [jlib::wrapper:getattr $vars y]

    if {$x == ""} {set x 0}
    if {$y == ""} {set y 0}

    foreach {x y} [TransformCoord $transform $x $y] break

    array set attrs $curAttrs
    array set attrs $vars
    set styles [split [jlib::wrapper:getattr $vars style] \;]

    foreach s $styles {
	foreach {attr val} [split $s :] break
	set attr [string trim $attr]
	set val [string trim $val]
	set attrs($attr) $val
    }

    set allopts [TextOpts]

    set opts [lindex $allopts 0]
    set fontopts [lindex $allopts 1]
    if {$fontopts != ""} {
	variable app_font
	set fontname [list font $fontopts]
	if {![info exists app_font($fontname)]} {
	    # create a font to match the settings
	    set app_font($fontname) [eval [list font create $fontname] $fontopts]
	}
	lappend opts -font $app_font($fontname)
    }

    Debug 2 text $x $y -text $chdata $opts
    eval [list $c create text $x $y -text $chdata] $opts
}

proc svg::LineOpts {} {
    upvar attrs attrs
    upvar c c
    set opts {-joinstyle miter -capstyle butt}
    foreach {attr val} [array get attrs] {
	switch -- $attr {
	    "" {}
	    stroke {lappend opts -fill [color $c $val]}
	    stroke-width {lappend opts -width $val}
	    stroke-linecap {
		switch -- $val {
		    round {lappend opts -capstyle round}
		    square {lappend opts -capstyle projecting}
		}
	    }
	    stroke-linejoin {
		switch -- $val {
		    round {lappend opts -joinstyle round}
		    bevel {lappend opts -joinstyle bevel}
		}
	    }
	    id {lappend opts -tags [list [list id $val]]}
	}
    }
    return $opts
}

proc svg::PolygonOpts {} {
    upvar attrs attrs
    upvar c c
    set opts {-joinstyle miter -fill "" -outline ""}
    foreach {attr val} [array get attrs] {
	switch -- $attr {
	    "" {}
	    fill {
		if {$val != "" && $val != "none"} {
		    lappend opts -fill [color $c $val]
		}
	    }
	    stroke {
		if {$val != "" && $val != "none"} {
		    lappend opts -outline [color $c $val]
		}
	    }
	    stroke-width {lappend opts -width $val}
	    stroke-linejoin {
		switch -- $val {
		    round {lappend opts -joinstyle round}
		    bevel {lappend opts -joinstyle bevel}
		}
	    }
	    id {lappend opts -tags [list [list id $val]]}
	}
    }
    return $opts
}

proc svg::CircleOpts {} {
    variable Smooth
    upvar attrs attrs
    upvar c c
    set opts {-joinstyle round -fill "" -outline ""}
    if {$Smooth} {
	lappend opts -smooth bezier
    }
    foreach {attr val} [array get attrs] {
	switch -- $attr {
	    "" {}
	    fill {
		if {$val != "" && $val != "none"} {
		    lappend opts -fill [color $c $val]
		}
	    }
	    stroke {
		if {$val != "" && $val != "none"} {
		    lappend opts -outline [color $c $val]
		}
	    }
	    stroke-width {lappend opts -width $val}
	    id {lappend opts -tags [list [list id $val]]}
	}
    }
    return $opts
}

proc svg::TextOpts {} {
    upvar attrs attrs
    upvar c c
    set opts {-anchor w}
    set fontopts {}
    foreach {attr val} [array get attrs] {
	switch -- $attr {
	    "" {}
	    fill {lappend opts -fill [color $c $val]}
	    font-family      {lappend fontopts -family $val}
	    font-size        {lappend fontopts -size $val}
	    font-size-adjust { # How to do this in Tk? }
	    font-stretch     { # How to do this in Tk? }
	    font-style       {	if {[string equal $val italic]
	    			 || [string equal $val oblique]} {
			       	   lappend fontopts -slant italic
			        }
			     }
	    font-variant     { # How to do this in Tk? }
	    font-weight      {	if {[string match bold* $val]} {
			       	   lappend fontopts -weight bold
			        }
			     }
	    text-decoration  {	foreach subval $val {
				    switch -- $subval {
					underline {
					    lappend fontopts -underline on
					}
					line-through {
					    lappend fontopts -overstrike on
					}
				    }
				 }
			     }
	    dx   { # How to do this in Tk? }
	    dy   { # How to do this in Tk? }
	    id {lappend opts -tags [list [list id $val]]}
	}
    }
    return [list $opts $fontopts]
}


proc svg::TransformPoints {transform raw_points} {
    # SVG spec says coordinate points can be separated by comma or
    # white space or comma-with-white-space
    # string map...    convert , to space
    # regsub...        condense multiple whitespaces to single space
    regsub -all {\s\s*} [string map {, { }} [string trim $raw_points]] { } points_str

    set p {}
    foreach {x y} [split $points_str] {
	eval lappend p [TransformCoord $transform $x $y]
    }
    return $p
}

proc svg::ParseTransform {s} {
    Debug 2 $s

    set t {}
    while {[regexp {(\w+)\s*\(([^\)]*)\)(.*)} $s temp transform param s]} {
	lappend t [list $transform [split $param ", "]]
    }

    Debug 1 $s $t
    return $t
}

proc svg::TransformCoord {transform x y} {
    Debug 2 $transform $x $y

    set matrix [list 1 0 0 1 0 0]

    foreach t $transform {
	foreach {op param} $t break
	switch -- $op/[llength $param] {
	    matrix/6 {
		set matrix [Tcompose $matrix $param]
	    }
	    translate/1 {
		foreach tx $param break
		set matrix [Tcompose $matrix [list 1 0 0 1 $tx 0]]
	    }
	    translate/2 {
		foreach {tx ty} $param break
		set matrix [Tcompose $matrix [list 1 0 0 1 $tx $ty]]
	    }
	    scale/1 {
		foreach sx $param break
		set matrix [Tcompose $matrix [list $sx 0 0 $sx 0 0]]
	    }
	    scale/2 {
		foreach {sx sy} $param break
		set matrix [Tcompose $matrix [list $sx 0 0 $sy 0 0]]
	    }
	    rotate/1 {
		foreach a $param break
		set a [expr {3.1415926 * $a / 180}]
		set matrix [Tcompose $matrix \
				    [list [expr {cos($a)}]  [expr {sin($a)}] \
					  [expr {-sin($a)}] [expr {cos($a)}] 0 0]]
	    }
	    rotate/3 {
		foreach {a cx cy} $param break
		set a [expr {3.1415926 * $a / 180}]
		set matrix [Tcompose $matrix [list 1 0 0 1 $cx $cy]]
		set matrix [Tcompose $matrix \
				    [list [expr {cos($a)}]  [expr {sin($a)}] \
					  [expr {-sin($a)}] [expr {cos($a)}] 0 0]]
		set matrix [Tcompose $matrix [list 1 0 0 1 [expr {-$cx}] [expr {-$cy}]]]
	    }
	    skewX/1 {
		foreach a $param break
		set a [expr {3.1415926 * $a / 180}]
		set matrix [Tcompose $matrix [list 1 0 [expr {tan($a)}] 1 0 0]]
	    }
	    skewY/1 {
		foreach a $param break
		set a [expr {3.1415926 * $a / 180}]
		set matrix [Tcompose $matrix [list 1 [expr {tan($a)}] 0 1 0 0]]
	    }
	}
    }

    return [Tapply $matrix $x $y]
}

proc svg::Tcompose {matrix1 matrix2} {
    Debug 2 [list $matrix1] [list $matrix2]

    
    foreach {a1 b1 c1 d1 e1 f1} $matrix1 break
    foreach {a2 b2 c2 d2 e2 f2} $matrix2 break

    set a [expr {$a1*$a2 + $c1*$b2}]
    set b [expr {$b1*$a2 + $d1*$b2}]
    set c [expr {$a1*$c2 + $c1*$d2}]
    set d [expr {$b1*$c2 + $d1*$d2}]
    set e [expr {$a1*$e2 + $c1*$f2 + $e1}]
    set f [expr {$b1*$e2 + $d1*$f2 + $f1}]

    return [list $a $b $c $d $e $f]
}

proc svg::Tapply {matrix x y} {
    Debug 2 [list $matrix] $x $y

    foreach {a b c d e f} $matrix break
    set x1 [expr {$a*$x + $c*$y + $e}]
    set y1 [expr {$b*$x + $d*$y + $f}]

    Debug 1 $x1 $y1
    return [list $x1 $y1]
}

proc svg::color {c color} {
    Debug 2 $color

    if {[catch {$c create line 0 0 0 0 -fill $color -width 0} id]} {
	return black
    } else {
	$c delete $id
	return $color
    }
}

proc svg::Debug {level args} {
    variable Debug

    if {$Debug >= $level} {
	puts "[lindex [info level -1] 0]: [join $args]"
    }
}

