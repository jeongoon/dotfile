function test_sub_file
    set coeff $argv[1]
    set offset $argv[2]
    set episode_num $argv[3]
    if exe_subs $coeff $offset $episode_num 
        set media (find_media_file "mkv" (make_season_episode_string $episode_num))
        if test -f $media
            mpv $media --sub-file=test.srt
        end
    else
        echo "test_sub_file: couldn't find media file: $media"
    end
end
