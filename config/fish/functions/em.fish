function em --description 'alias em emacs -nw $argv'
	emacs -nw $argv
end

function mc --description 'alias mc emacsclient $argv'
	emacsclient -tc $argv
end
