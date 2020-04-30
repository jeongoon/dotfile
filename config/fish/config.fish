#set -g theme_nerd_fonts yes
set -g theme_color_scheme gruvbox
set -g fish_color_autosuggestion 999 brblack

if test -f /etc/debian_version
    abbr AI sudo apt-get install
    abbr AU sudo apt-get update
else if test -f /etc/arch-release
    abbr SP sudo pacman
end

abbr SU sudo -i
abbr SS sudo systemctl

#set -x EDITOR /usr/bin/vim # done by ~/.pam_environment

if false
    set PATH ~/bin/texbin $HOME/perl5/bin ~/bin ~/.rakudo/install/bin ~/.rakudo/install/share/perl6/site/bin $PATH

    set BC_ENV_ARGS "-q"
    set -q PERL5LIB; and set -x PERL5LIB $HOME/perl5/lib/perl5:$PERL5LIB;
    set -q PERL5LIB; or set -x PERL5LIB $HOME/perl5/lib/perl5;
    set -q PERL_LOCAL_LIB_ROOT; and set -x PERL_LOCAL_LIB_ROOT $HOME/perl5:$PERL_LOCAL_LIB_ROOT;
    set -q PERL_LOCAL_LIB_ROOT; or set -x PERL_LOCAL_LIB_ROOT $HOME/perl5;
    set -x PERL_MB_OPT --install_base\ \"$HOME/perl5\";
    set -x PERL_MM_OPT INSTALL_BASE=$HOME/perl5;

# PERL6LIB use comma(,) as a seporator
   set -x myPerl6 $HOME/proj/myPerl6;
   set -q PERL6LIB; and echo $PERL6LIB | egrep -q "\b$myPerl6\b"
   set -q PERL6LIB; or set -x PERL6LIB $myPerl6
     and set -x PERL6LIB $myPerl6 $PERL6LIB;

   set -x WINEPREFIX $HOME/.wine
   set -x WINEARCH win64
end
