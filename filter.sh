# Language
lang=OCaml
ext=ocaml

# Do not modify from here on

clone=0
filtermove=0
typemove=0

while :; do
	case $1 in
		-c)
			clone=1
			;;
		-f)
			filtermove=1
			;;
		-t)
			typemove=1
			;;
		*)               # Default case: No more options, so break out of the loop.
			break
		esac
	shift
done

if [ $clone -eq 1 ]
then
# Clone the repo
rm -Rf RosettaCodeData
git clone https://github.com/acmeism/RosettaCodeData.git

# Remove useless folders
cd RosettaCodeData; rm -Rf Conf Lang Meta README.md; cd Task
fi

if [ $filtermove -eq 1 ]
then
# Remove non-$lang implementations and move $lang files to the top
for d in `ls .`; do
    echo -n "Processing $d.";
    for sd in `ls $d -I $lang`; do
		echo "$d/$sd";
		echo -n " ";
		rm -Rf "$d/$sd";
		done;
	for sd in `ls $d/$lang`; do
		mv "$d/$lang/$sd" "../$sd"
		done;
	rm -Rf $d
	done;

# Again, fix folders
cd ..; rm -Rf Task
fi

if [ $typemove -eq 1 ]
then

if [ -d "RosettaCodeData" ]
then
	cd RosettaCodeData;
fi

mkdir OK

# Finally run the analyser on each file, expect 0 on success.
for f in `ls *.$ext`; do
	echo -n "Typing $f.";
	../main.byte "$f" "$f"
	if [ $? -eq 0 ]
	then
		echo "OK";
		mv {,"OK/"}$f
	else
		echo "KO";
		#mv {,"KO/"}$f
	fi
	done;
fi