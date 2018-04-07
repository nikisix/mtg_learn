#MTG LEARN
Use mtg as testbed for nlp based deep reinforcement learning.
Pull requests and contributors welcome.

#Download allcards command
Need to download allcards.json for code to run
wget http://mtgjson.com/json/AllCards.json.zip -O temp.zip; unzip -p temp.zip | python -m json.tool > allcards.json; rm temp.zip

#Search through allcards.json (using sed)
sed -n /Black\ Lotus/,/},/p allcards.json
sed -n /Ranging Raptors/,/},/p allcards.json
