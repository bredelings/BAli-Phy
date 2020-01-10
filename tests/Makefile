clean:
	find . -name obtained-* | xargs rm -f
	find . -name rb-obtained-* | xargs rm -f
	find . -name token*.dot | xargs rm -f
	find . -name ignore-* | xargs rm -rf
	find . -name history.txt | xargs rm -rf
	find . -name BAliPhy.Main.hs | xargs rm -rf
	find . -name Print.Main.hs | xargs rm -rf


moreclean: clean
	find . -name '*~' | xargs rm -f
