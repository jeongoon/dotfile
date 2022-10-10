function apply_tested_sub_file
    set episode_num $argv[1]
    if ! set -q MEDIA_SUB_TYPE; or test (string length $MEDIA_SUB_TYPE) -lt 1
        help_media_sub_type "apply_tested_sub_file" "MEDIA_SUB_TYPE"
        echo "apply_tested_sub_file: quit."
        return 1
    end

    if ! set -q MEDIA_TYPE; or test (string length $MEDIA_TYPE) -lt 1
        help_media_type "apply_tested_sub_file" "MEDIA_TYPE"
        echo "apply_tested_sub_file: quit."
        return 2
    end

    set media_file (find_media_file $MEDIA_TYPE (make_season_episode_string $episode_num))

    if test -f $media_file
        set sub_file (string replace --regex "\.$MEDIA_TYPE\$" ".$MEDIA_SUB_TYPE" $media_file)
    else
        set sub_file (find_media_file $MEDIA_SUB_TYPE (make_season_episode_string $episode_num))
        if ! test -f  $sub_file
            echo "apply_tested_sub_file: couldn't guess any subtitle name: quit."
            return 3
        end
    end
    echo "apply_tested_sub_file: test.srt -> $sub_file"
    mv -i "test.srt" $sub_file
end
