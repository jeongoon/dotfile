function find_media_files
    set media_type $argv[1]
    set keyword $argv[2]
    find . -maxdepth 1 -iname (string join "" "*" $keyword "*." $media_type)
end

function find_media_file
    set media_type $argv[1]
    set keyword $argv[2]
    set subs (find_media_files $media_type $keyword)
    if test (count $subs) -eq 1
        echo $subs
    else
        echo "found more than 1 subtitles or none: $subs" >&2
        return 1
        # fixme: to choose if it is not a interactive mode???
    end
end
