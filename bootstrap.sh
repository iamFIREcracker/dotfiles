# Dns
read -p "Configure DNS? [N/y] "
if [ "${REPLY}" = "y" ]; then
    sudo echo "192.168.0.4 code.matteolandi.net www.matteolandi.net" >> /etc/hosts
fi

# Misc
sudo apt-get install \
    vim-gtk \
    subversion \
    mercurial \
    git \
    bash-completion \
    openssh-server \
    gconf-editor \
    ncurses-term \
    markdown \
    nautilus-dropbox \
    inotify-tools \
    exuberant-ctags \
    sqlite3 \
    tree \
    fortune \
    cowsay \
    tree \
    texlive \
    texlive-latex-extra \
    gtk2-engines-pixbuf \
    nautilus-open-terminal \
    dvtm \
    bc \
    ack

sudo apt-get purge \
    indicator-messages


# Python
sudo apt-get install python-pip python-docutils
sudo pip install pygments

# Gemz
sudo apt-get install rubygems
sudo gem install lolcat bcat

# Initialize workspace
mkdir -p ~/workspace

svn co svn+ssh://svn@code.matteolandi.net/configs/branches/matteolandi ~/workspace/configs
(
cd ~/workspace/configs
bash install.sh home
profiles=$(gconftool-2 --get /apps/gnome-terminal/global/profile_list)
gconftool-2 --set /apps/gnome-terminal/global/profile_list \
  "${profiles%%]*},Custom]"
gconftool-2 --set /apps/gnome-terminal/global/default_profile Custom
)
gsettings set org.gnome.settings-daemon.peripherals.input-devices hotplug-command "/home/matteo/bin/trackpad_settings.sh"

git clone git@github.com:iamFIREcracker/productivity.git ~/workspace/productivity
git clone git@github.com:iamFIREcracker/onchange.git ~/workspace/onchange
svn co svn+ssh://svn@code.matteolandi.net/hacks/svn-tools ~/workspace/svn-tools
hg clone ssh://hg@bitbucket.org/sjl/hg-prompt ~/workspace/hg-prompt

# Mutt
sudo apt-get install mutt-patched msmtp offlineimap urlview
sudo pip install goobook
read -p "Enter password: " password
for f in .msmtprc .offlineimaprc .netrc; do
    sed -i "s/PASSWORD/${password}/" ~/workspace/configs/home/${f}
done
