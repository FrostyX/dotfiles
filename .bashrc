# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


export EDITOR=vim
export PATH="$PATH:/home/jkadlcik/.cask/bin"
export PATH="$PATH:/home/jkadlcik/node_modules/.bin"


alias v="vagrant"
alias dco="docker-compose"
alias undo="git reset --soft HEAD~1"
alias killjobs="for x in `jobs -p`; do kill -9 $x; done"
alias youtube-mp3="youtube-dl --extract-audio --audio-format mp3"


singlehead () {
    cmd="xrandr"
    cmd+=" --output DP-1-1 --off"
    cmd+=" --output DP-1-2 --off"
    cmd+=" --output DP-1-3 --off"
    cmd+=" --output DP-2 --off"
    cmd+=" --output eDP-1 --auto"
    $cmd
}

projector () {
    cmd="xrandr"
    cmd+=" --output eDP-1 --auto --primary"
    cmd+=" --output DP-1 --auto --same-as eDP-1"
    $cmd
}

multihead () {
    cmd="xrandr"
    cmd+=" --output eDP-1 --off"
    cmd+="--output DP-1-2 --auto"
    $cmd
}

3head () {
    cmd="xrandr"
    cmd+=" --output eDP-1 --off"
    cmd+=" --output DP-2 --auto"
    cmd+=" --output DP-1-1 --auto --right-of DP-2"
    cmd+=" --output DP-1-2 --auto --right-of DP-1-1"
    $cmd
}


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