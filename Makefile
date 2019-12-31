download-dictionaries:

	# Wiktionary
	curl https://dumps.wikimedia.org/enwiktionary/20191101/enwiktionary-20191101-all-titles.gz > ./wiktionary.gz
	unzip ./wiktionary.gz
	rm ./wiktionary.gz

	# Webster's Dictionary
	curl https://github.com/dwyl/english-words/raw/master/words.txt > dictionary.txt

	# Top 20k Words
	curl https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt > top.txt

	# Top 300k Words
	curl https://norvig.com/ngrams/count_1w.txt > top-full.txt
	sed -i '' 's/[0-9]*//g' top-full.txt