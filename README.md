# dotfiles
Diverse Konfigurationen

## Auf neuem System

* in .bashrc `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'` einfügen
* `git clone --bare git@github.com:sfieger/dotfiles.git $HOME/dotfiles --recurse-submodules`
* `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'`
* `config checkout`
    * bei Fehler:
    * `mkdir -p .config-backup && config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} xargs -I{} mv {} .config-backup/{}`
    * `config checkout`
* `config config --local status.showUntrackedFiles no`
