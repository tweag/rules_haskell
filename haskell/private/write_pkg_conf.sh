cat $1 > $4
echo "exposed-modules: `cat $2`" >> $4
deps=$(cat $(< $3) | tr '\n' " ")
echo "depends: $deps" >> $4
