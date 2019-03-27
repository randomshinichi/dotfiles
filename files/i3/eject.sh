target=$(df -hx tmpfs -x squashfs --output=source,target | dmenu -l 10)
# output will look like
# /dev/sdb2      /home 
# so we have to split this

targetarray=($target)
echo "unmounting ${targetarray[0]}"
udiskie-umount ${targetarray[0]}
