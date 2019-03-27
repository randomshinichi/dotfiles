target=$(locate /home/shinichi /data | dmenu -l 40 -i)

if [ ! -z "${target}" ]
then
    echo "found an answer... proceeding"
    if [ -f "$target" ]
    then
	echo "it's a file"
	rifle "$target" & disown
    else
	echo "it's a directory"
	nautilus "$target" & disown
    fi
fi
