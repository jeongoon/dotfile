function help_media_variable
    set parent_function $argv[1]
    set var_name $argv[2]
    echo "$parent_function: set $var_name like:" >&2
    echo "    set -gx (or -U) $var_name \"srt\"" >&2
end

function help_media_sub_type
    help_media_variable $argv[1] "MEDIA_SUB_TYPE"
end

function help_media_type
    help_media_variable $argv[1] "MEDIA_TYPE"
end
