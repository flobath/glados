#!/usr/bin/env bash

failed_tests=0

if [ ! -x "$GLaDOS_EXEC_PATH" ]; then
    echo 1>&2 "$GLaDOS_EXEC_PATH: invalid executable"
    exit 1
fi
if [ ! -d "$TEST_CASES_DIRECTORY" ]; then
    echo 1>&2 "$TEST_CASES_DIRECTORY: Invalid test directory"
    exit 1
fi

for test_dir in "$TEST_CASES_DIRECTORY"/*; do
    expected_exit_status=0
    test_name=$(basename "$test_dir: ")

    echo -n "$test_name"
    ([ -r "$test_dir/in" ] || echo 1>&2 "  $test_name: Missing stdin file") &&
    ([ -r "$test_dir/out" ] || echo 1>&2 "  $test_name: Missing stdout file") || continue
    if [ -r "$test_dir/exit" ]; then
        expected_exit_status="$(cat $test_dir/exit)"
    fi

    # weird trick described in https://unix.stackexchange.com/questions/383217/shell-keep-trailing-newlines-n-in-command-substitution
    result="$("$GLaDOS_EXEC_PATH" < "$test_dir/in" 2> /dev/null; ret=$?; echo .; exit $ret)"
    exit_status=$?
    result=${result%.}

    if [ "$exit_status" -ne "$expected_exit_status" ]; then
        echo -e ' \e[1;31m✘\e[0m'
        echo -e 1>&2 "  Failed: invalid exit status \e[1;31m$exit_status\e[0m; expecting \e[1;37m$expected_exit_status\e[0m"
        ((++failed_tests))
        continue
    fi

    if ! diff <(echo -n "$result") "$test_dir/out" > /dev/null; then
        echo -e ' \e[1;31m✘\e[0m'
        echo 1>&2 '  Failed: expecting standard output'
        echo 1>&2 '=================='
        cat 1>&2 "$test_dir/out"
        echo 1>&2 '=================='
        echo 1>&2 '  But got:'
        echo 1>&2 '=================='
        echo -n 1>&2 "$result"
        echo 1>&2 '=================='
        ((++failed_tests))
        continue
    fi

    echo -e '\e[1;32m✔\e[0m'
done

echo
echo "$failed_tests failed."
exit $((failed_tests>255 ? 255 : failed_tests))
