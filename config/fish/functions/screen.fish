set screenrc $HOME/.config/screen/rc

function screen --description 'alias screen screen -f $screenrc'
    if test -f $screenrc
        /usr/bin/screen -c $screenrc -R $argv
    else
        echo "could not find resource file: $screenrc: run without it."
        /usr/bin/screen -R $argv
    end
end
