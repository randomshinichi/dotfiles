# https://github.com/micheleg/dash-to-dock/issues/781
# There are many more settings in dconf than are exposed in the Extensions settings GUI.
for i in {1..9}
do
    gsettings set org.gnome.shell.extensions.dash-to-dock app-ctrl-hotkey-$i "[]"
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-$i "['<Super>$i']"
done
