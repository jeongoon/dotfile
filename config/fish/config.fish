set -g theme_nerd_fonts yes # if you set by -g once, don't need to set again.
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

set -q GOPATH; and set -x GOPATH $HOME/proj/golib
set -x s_path_basic bin sbin bin/texbin perl5/bin
set -x s_path_raku  .rakudo/install/bin \
    .rakudo/install/share/perl6/site/bin .local/share/rakudo/bin \
    .local/share/rakudo/share/perl6/site/bin
set -x s_path_golang .local/share/go/bin $GOPATH/bin

for di in $s_path_basic $s_path_raku $s_path_golang
    if test -d $HOME/$di
        string match -vq $HOME/$di "$PATH"; and set -x PATH $HOME/$di $PATH
    end
end

set BC_ENV_ARGS "-q"

if test -d $HOME/perl5
    set -q PERL5LIB; and set -x PERL5LIB $HOME/perl5/lib/perl5:$PERL5LIB;
    set -q PERL5LIB; or set -x PERL5LIB $HOME/perl5/lib/perl5;
    set -q PERL_LOCAL_LIB_ROOT; and set -x PERL_LOCAL_LIB_ROOT $HOME/perl5:$PERL_LOCAL_LIB_ROOT;
    set -q PERL_LOCAL_LIB_ROOT; or set -x PERL_LOCAL_LIB_ROOT $HOME/perl5;
    set -x PERL_MB_OPT --install_base\ \"$HOME/perl5\";
    set -x PERL_MM_OPT INSTALL_BASE=$HOME/perl5;
end

# PERL6LIB use comma(,) as a seporator

for di in $HOME/lib $HOME/proj/myPerl6
    if test -d $di
        if string match "" "$PERL6LIB"
            set -x PERL6LIB $di
        else
            string match -vq $di "$PERL6LIB"; and set -x PERL6LIB $di,$PERL6LIB
        end
    end
end

if test -d $HOME/.wine
    set -x WINEPREFIX $HOME/.wine
    set -x WINEARCH win64
end
