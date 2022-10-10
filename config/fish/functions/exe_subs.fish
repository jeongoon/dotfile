function exe_subs
    if test (count $argv) -lt 3 
        echo "usage: exe_subs <coeff> <offset> <episode_num>" >&2
        return 1
    end

    set episode_num $argv[3]
    and set sub (find_sub_file $episode_num)
    if test ! -r "$sub"
        echo "$sub: does not exist"
        return 2
    end

    echo "changing $sub ..."
    set coeff $argv[1]
    set offset $argv[2]
    if subs -a $coeff -b $offset $sub -o "test.srt"
        echo "done."
    else
        echo "exe_subs: subs exit abnormally: quit."
        return $status
    end
end
