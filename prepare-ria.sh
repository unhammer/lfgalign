#!/bin/sh

# Usage:
# ./prepare-ria.sh /usr/share/dict/de-en.txt /path/to/ria_12_06_09


if [ $# -ne 2 ];
then
    echo "Usage:"
    echo ""
    echo "\$ $0 /usr/share/dict/de-en.txt /path/to/ria_12_06_09"
    echo ""
    echo "where the first argument is the path to the Ding de-en dictionary, and the second to the source directory of the RIA Open Source Rule Induction Tool"
    echo "See http://www-user.tu-chemnitz.de/~fri/ding/ and http://www.computing.dcu.ie/~ygraham/software.html, respectively."
    exit 0;
fi

echo "Extracting Ding dictionary... you'll probably see a couple of warnings."
cat "$1" | python2 dev/ding-to-LPT.py > dev/de-en-LPT.pl
echo ""
echo "Done."

echo "Creating RIA symlink and extracting analyses..."
rm -f eval/ria
ln -s "$2/data" eval/ria
cd eval/ria/alignments
tar xzf sents_0000.tar.gz 
tar xzf sents_0001.tar.gz 
tar xzf sents_0002.tar.gz 
tar xzf sents_0003.tar.gz 
cd ../sl_train
tar xzf sents_0000.tar.gz 
tar xzf sents_0001.tar.gz 
tar xzf sents_0002.tar.gz 
tar xzf sents_0003.tar.gz 
cd ../tl_train
tar xzf sents_0000.tar.gz 
tar xzf sents_0001.tar.gz 
tar xzf sents_0002.tar.gz 
tar xzf sents_0003.tar.gz 
cd ../../..

echo "Done."
echo "Try running #'ev-ria on #'ria-analyses and #'ding-lpt in lfgalign."
