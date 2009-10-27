# This is a configuration file for tkabber used by Andriy Senkovych. The
# features include:
# * using custom color theme
# * using smileys
# * 

# Reading account information from external file.
source $::configdir/passwd.tcl

# Using tabbed interface
set ifacetk::options(use_tabbar) 1

# Setting color theme
option readfile $::configdir/themes/black.xrdb userDefault

# Postload settings:
#
# * Enable nested groups in the roster. This can be used to set 
#   multiple accounts to one person.
# * Enable filtering by name in roster
# * Announce and share user's avatar
# 
proc postload {} {
	avatar::load_file $::configdir/avatar.gif
	set avatar::options(announce) 1
	set avatar::options(share) 1
}

proc finload {} {
	set ::plugins::tktray::options(enable) 1
	set ::ifacetk::systray::options(display_status) 1
	set ::ifacetk::systray::options(blink) 0

	set ::chat::options(smart_scroll) 1
	set ::plugins::options(timestamp_format) {[%T]}
	set ::plugins::xhtml::options(enable) 0
	set ::muc::options(propose_configure) 1

	set ::plugins::conferenceinfo::options(autoask) 1

	set ::ft::options(download_dir) $::env(HOME)
	set ::ifacetk::options(closebuttonaction)  systray

	set ::plugins::ping::options(ping) 1

	set ::plugins::ispell::options(enable) 1
	set ::plugins::ispell::options(dictionary_encoding) {koi8-u}
	set ::ifacetk::roster::show_only_online 1
	set ::ifacetk::roster::show_transport_icons 1
	set ::ifacetk::roster::show_transport_user_icons 1
	set ::ifacetk::roster::options(nested) 1
	set ::ifacetk::roster::options(chats_group) 1
	set ::ifacetk::roster::options(use_filter) 1

	set ::sound::options(mute) 1
	set ::sound::options(mute_if_focus) 1
	set ::sound::options(external_play_program) {aplay}

	set ::plugins::session::options(save_on_exit) 1
	set ::plugins::session::options(open_on_start) 1

	set ::::tls_warnings 0
}
