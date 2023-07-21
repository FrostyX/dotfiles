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

# TODO Use this instead
# https://wiki.archlinux.org/title/Session_lock
alias suspend="i3lock -i ~/.dotfiles/.config/qtile/img/bsod.png && systemctl suspend"


singlehead () {
    cmd="xrandr"
    cmd+=" --output DP-1-1 --off"
    cmd+=" --output DP-1-2 --off"
    cmd+=" --output DP-1-3 --off"
    cmd+=" --output DP-3-1 --off"
    cmd+=" --output DP-3-2 --off"
    cmd+=" --output DP-3-3 --off"
    cmd+=" --output DP-2 --off"
    cmd+=" --output DP-3 --off"
    cmd+=" --output DP-4 --off"
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
    case $HOSTNAME in
        "zeratul")
            L="DP-4"
            M="DP-3-1"
            R="DP-3-2"
            ;;
        "alarak")
            L="DP-2"
            M="DP-1-1"
            R="DP-1-2"
            ;;
        *) echo "Unrecognized hostname"; exit 1;;
    esac

    cmd="xrandr"
    cmd+=" --output eDP-1 --off"
    cmd+=" --output $L --auto"
    cmd+=" --output $M --auto --right-of $L"
    cmd+=" --output $R --auto --right-of $M"
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
