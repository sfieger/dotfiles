# dotfiles
Diverse Konfigurationen

## Auf neuem System

* in .bashrc/.zshrc `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'` einfügen
* `echo "dotfiles" >> .gitignore`
* `git clone --bare git@github.com:sfieger/dotfiles.git $HOME/dotfiles`
* `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'`
* `config checkout`
    * bei Fehler:
    * `mkdir -p dotfiles-backup && config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} dotfiles-backup/{}`
    * `config checkout`
* `config submodule update --init --recursive`
* `config config --local status.showUntrackedFiles no`
