# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


export EDITOR=vim
export PATH="$PATH:/home/jkadlcik/.cask/bin"
export PATH="$PATH:/home/jkadlcik/node_modules/.bin"


# GIT
alias undo="git reset --soft HEAD~1"

# Monitor multihead
alias singlehead="xrandr --output DP-1-1 --off --output DP-1-2 --off --output DP-1-3 --off --output DP-2 --off --output eDP-1 --auto"
alias projector="xrandr --output eDP-1 --auto --primary --output DP-1 --auto --same-as eDP-1"
alias multihead="xrandr --output eDP-1 --off --output DP-1-2 --auto"
alias 3head="xrandr --output eDP-1 --off --output DP-2 --auto --output DP-1-1 --auto --right-of DP-2 --output DP-1-2 --auto --right-of DP-1-1"


alias v="vagrant"
alias dco="docker-compose"
alias youtube-mp3="youtube-dl --extract-audio --audio-format mp3"
alias killjobs="for x in `jobs -p`; do kill -9 $x; done"


pyclean () {
    sudo find . -type f -name "*.py[co]" -delete
    sudo find . -type d -name "__pycache__" -delete
}

make_prompt () {
    default="\[\e[0m\]"
    red="\[\e[31m\]"
    green="\[\e[32m\]"
    color="\`if [ \$? = 0 ]; then echo $green; else echo $red; fi\`"

    start=""
    middle="[\u@\h \W]"
    end="$color\$$default"
    PS1="${start}${middle}${end} "
}
make_prompt
