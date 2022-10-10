function find_sub_file
    if ! set -q MEDIA_SUB_TYPE
        help_media_sub_type "find_sub_file"
        echo "find_sub_file: quit." >&2
        return 1
    end

    set episode_num $argv[1]
    set season_episode (make_season_episode_string $episode_num)
    if test $status -ne 0
        echo "find_sub_file: couldn't get season and episode number: quit" >&2
        return
    end
    find_media_file $MEDIA_SUB_TYPE $season_episode
end
