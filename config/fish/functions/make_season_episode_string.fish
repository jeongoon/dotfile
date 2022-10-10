function make_season_episode_string
    if set -q MEDIA_SEASON
        set episode_num $argv[1]
        if test (string length $episode_num) -ge 1
            string join "" "S" $MEDIA_SEASON "E" $episode_num
        else
            echo "make_season_episode_string <episode_num>" >&2
            return 1;
        end
    else
        echo "please set MEDIA_SEASON like: set -gx (or -U) MEDIA_SEASON \"02\"" >&2
        return 2;
    end
end    
