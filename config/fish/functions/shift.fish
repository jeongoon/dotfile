set -l PROG_NAME "shift"
set -l PROG_DESCRIPTION 'echo first element from the given list name and remove fron the list'

set -l ERROR_CODES \
    "1:No list name given" \
    "2:Too many argument" \
    "10:The list does not exist" \
    "11:The list is empty"

function shift -d $PROG_DESCRIPTION \
    -S \
    -V PROG_NAME \
    -V ERROR_CODES

    set -l errormsg ""

    shift_sanity_check errormsg $argv
    set -l errorcode $status

    if test $errorcode -ne 0
        if test -n $errormsg
            shift_print_error_ $errormsg
        end
        shift_print_usage_
        return $errorcode
    end


    # do main job
    set len (count $$argv[1])
    if test $len -le 0
        shift_print_error_ (echo_errormsg_of_ 11 ERROR_CODES)
        return 11
    end

    echo $$argv[1][1]
    set $argv[1] $$argv[1][2..-1]
end

function shift_print_error_ -S
    for e in $argv
        echo "$PROG_NAME: $e" >&2
    end
end

# -S: makes read variables from caller.
function shift_print_usage_ -S
    echo -n \
"usage: $PROG_NAME <\"a\" list name>
        will get the first element from the list named <\"a\" list name>.
        If the list is empty, it will return error as well.
" >&2

end

# -S: makes read variables from caller.
function shift_sanity_check -S
    set -l ret_varname ""

    if test -n $argv[1]
        set ret_varname $argv[1]
        set $ret_varname ""
    else
        set ret_varname "_ignored_"
    end

    set -l argc (count $argv[2..-1])

    if test $argc -gt 1
        set $ret_varname (echo_errormsg_of_ 2 ERROR_CODES)
        return 2
    end

    if test $argc -ne 1
        set $ret_varname (echo_errormsg_of_ 1 ERROR_CODES)
        return 1
    end

    if ! set -q $argv[2]
        set $ret_varname (echo_errormsg_of_ 10 ERROR_CODES)
        return 10
    end

    return 0
end

function echo_errormsg_of_ -S
    if ! set -q argv[1]
        return -1
    end

    # limit the scope of some variables
    set -l errnum $argv[1]
    set -l error_codes_from

    if test -n "$argv[2]"
        set error_codes_from $argv[2]
    else
        set error_codes_from "ERROR_CODES"
    end

    for e in $$error_codes_from
        set -l epair (string split -m1 ":" $e)
        if test $epair[1] -eq $errnum
            echo $epair[2]
            return 0
        end
    end

    # return non zero if could not find
    return 1
end
