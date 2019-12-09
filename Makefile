all: day1 day2 day3 day4 day5 day6 day7
day1:
	runhaskell day1.hs < day1.input
day2:
	runhaskell day2.hs < day2.input
day3:
	runhaskell day3.hs < day3.input
day4:
	runhaskell day4.hs
day5:
	echo -n "Part One: "; echo 1 | runhaskell day5.hs day5.input
	echo -n "Part Two: "; echo 5 | runhaskell day5.hs day5.input
day6:
	runhaskell day6.hs < day6.input
day7:
	runhaskell day7.hs < day7.input
watch:
	while inotifywait -e close_write day$(day).hs; do make day$(day); done
