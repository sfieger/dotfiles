alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls="ls --color=auto"

alias grep='grep --color=auto'

alias mkdir='mkdir -pv'

eval `dircolors ~/.dircolors`

alias mv='mv -i'
alias rm='rm -i'
alias cp='cp -i'
alias ln='ln -i'

alias ..='cd ..'

alias vim='vim -c "let g:tty='\''$(tty)'\''"'
