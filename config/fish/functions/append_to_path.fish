function append_to_path -d 'append given path into PATH environment variable with checking '
    if ! elem $argv[1] $PATH
	set -x PATH $PATH $argv[1]
    end
end

function append_to_path_orig -d 'append given path into PATH environment variable with checking '
    if test -d $argv[1] && string match --regex --invert --quiet ":*"$argv[1]":*" $PATH
	set -x PATH $PATH $argv[1]
    end
end

function make_uniq_path
    #set -x PATH (for p in $PATH; echo $p; end | uniq_)
end
