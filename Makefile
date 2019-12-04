all: day1 day2 day3 day4
day1:
	runhaskell day1.hs < day1.input
day2:
	runhaskell day2.hs < day2.input
day3:
	runhaskell day3.hs < day3.input
day4:
	runhaskell day4.hs

watch:
	while inotifywait -e close_write day$(day).hs; do make day$(day); done
