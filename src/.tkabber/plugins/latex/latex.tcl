# $Id: latex.tcl 1330 2007-12-19 20:53:27Z sergei $
# Shows LaTeX formulas as pictures (*NIX and Windows only)
# Requires working latex, dvips, imagemagick
# and their binaries should be in the directory listed in PATH

package require msgcat

namespace eval latex {
    variable scriptdir [file dirname [info script]]

    ::msgcat::mcload [file join $scriptdir msgs]

    variable tmpdir [pwd]
    variable null ""
    switch -- $tcl_platform(platform) {
	windows {
	    catch { set tmpdir $::env(TMP) }
	    catch { set tmpdir $::env(TEMP) }
	    set null "2>NUL"
	}
	unix {
	    set tmpdir "/tmp"
	    catch { set tmpdir $::env(TMPDIR) }
	    set null "2>/dev/null"
	}
	macintosh {
	    # In case when someone would like to implement it
	    set tmpdir $::env(TRASH_FOLDER)
	    return
	}
    }

    custom::defgroup Plugins [::msgcat::mc "Plugins options."] -group Tkabber

    custom::defgroup LaTeX [::msgcat::mc "LaTeX formulas plugin options."] -group Plugins

    custom::defvar options(resolution) 100  \
	[::msgcat::mc "Resolution of the LaTeX generated images in pixels per inch."] \
	-group LaTeX -type integer

    # Blacklist is taken from KopeTeX. Not all of them are dengerous though
    variable blacklist {\def \let \futurelet \newcommand \renewcommand
			\else \fi \write \input \include \chardef
			\catcode \makeatletter \noexpand \toksdef
			\every \errhelp \errorstopmode \scrollmode
			\nonstopmode \batchmode \read \csname
			\newhelp \relax \afterground \afterassignment
			\expandafter \noexpand \special \command \loop
			\repeat \toks \output \line \mathcode \name
			\item \section \DeclareRobustCommand}
}

proc latex::find_formulas {chatid from type body x} {
    variable blacklist

    init_colors $chatid

    foreach {str formula} [regexp -all -inline {\$\$([^$]+)\$\$} $body] {
	if {[lsearch -exact [image names] latex/$str] >= 0} continue

	set bl 0
	foreach csname $blacklist {
	    if {[string first $csname $formula] >= 0} {
		set bl 1
		break
	    }
	}
	if {$bl} continue

	if {[catch {convert_formula latex/$str $formula} msg]} {
	    debugmsg plugins "latex error: $msg"
	    continue
	}

	plugins::emoticons::add $str latex/$str
    }
}

hook::add draw_message_hook [namespace current]::latex::find_formulas 10

proc latex::convert_formula {imgname formula} {
    variable tmpdir
    variable options
    variable bg
    variable fg
    variable null

    set res $options(resolution)

    set i 0
    while {1} {
	set dirname [file join $tmpdir latex$i]
	if {![file exists $dirname]} break
	incr i
    }

    file mkdir $dirname
    set fname [file join $dirname out.tex]

    set fd [open $fname w]
    fconfigure $fd -encoding utf-8
    puts $fd "\\documentclass\[12pt,letterpaper\]{article}
	      \\usepackage{amsmath}
	      \\usepackage{amssymb}
	      \\usepackage{color}
	      \\pagestyle{empty}
	      \\pagecolor\[rgb\]{$bg}
	      \\begin{document}
	      \\color\[rgb\]{$fg}
	      \\begin{gather*}$formula\\end{gather*}
	      \\end{document}"
    close $fd

    set wd [pwd]
    if {[catch {
	    cd $dirname
	    exec latex -interaction=batchmode out.tex $null
	    exec dvips -o [file join $dirname out.eps] \
		       -E \
		       [file join $dirname out.dvi] $null
	    exec convert +adjoin \
			 -antialias \
			 -density ${res}x${res} \
			 [file join $dirname out.eps] \
			 [file join $dirname out.gif] $null
	} msg]} {
	cd $wd
	file delete -force $dirname
	return -code error $msg
    }

    image create photo $imgname -file [file join $dirname out.gif]
    cd $wd
    file delete -force $dirname

    # If the page is empty, dvips doesn't crop it, so we
    # check image height and width. Letter paper dimensions are 8.5in x 11in
    if {abs([image width $imgname] - [expr $res * 8.5]) < 1 && \
	    abs([image height $imgname] - [expr $res * 11]) < 1} {
	# It's better to delete image and run latex every time when the same
	# formula appears. Otherwise large images will be stored
	# and waste space
	image delete $imgname
	return -code error "Image is empty"
    }

    return $imgname
}

proc latex::init_colors {chatid} {
    variable fg
    variable bg

    if {[info exists fg] && [info exists bg]} return

    set chatw [chat::chat_win $chatid]

    set bgopt [option get $chatw background Chat]
    if {$bgopt == ""} {
	set bgopt white
    }
    set bg [get_rgb_color $chatw $bgopt]
    
    set fgopt [option get $chatw foreground Chat]
    if {$fgopt == ""} {
	set fgopt black
    }
    set fg [get_rgb_color $chatw $fgopt]
}

proc latex::get_rgb_color {w color} {
    lassign [winfo rgb $w $color] r g b
    set r [expr {($r % 256)/256.}]
    set g [expr {($g % 256)/256.}]
    set b [expr {($b % 256)/256.}]
    return $r,$g,$b
}

