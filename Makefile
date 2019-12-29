download-dictionaries:

	# Wiktionary
	wget https://dumps.wikimedia.org/enwiktionary/20191101/enwiktionary-20191101-all-titles.gz -O ./wiktionary.gz
	unzip ./wiktionary.gz
	rm ./wiktionary.gz

	# Webster's Dictionary
	wget https://github.com/dwyl/english-words/raw/master/words.txt -O dictionary.txt