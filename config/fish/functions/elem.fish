function elem -d 'find first value is in the list of rest values'
    set found 0
    for arg in $argv[2..-1]
        if test $found -eq 0 && test $arg = $argv[1]
            set found 1
            break
        end
    end

    if test $found -eq 1
        true
    else
        false
    end
end
