# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


export EDITOR=vim
export PATH="$PATH:/home/jkadlcik/.local/bin"
export PATH="$PATH:/home/jkadlcik/.cask/bin"
export PATH="$PATH:/home/jkadlcik/node_modules/.bin"


alias v="vagrant"
alias dco="docker-compose"
alias undo="git reset --soft HEAD~1"
alias killjobs="for x in `jobs -p`; do kill -9 $x; done"
alias youtube-dl="yt-dlp"
alias youtube-mp3="youtube-dl --extract-audio --audio-format mp3"
alias untar="tar -xvzf"

# TODO Use this instead
# https://wiki.archlinux.org/title/Session_lock
alias suspend="i3lock -i ~/.dotfiles/.config/qtile/img/bsod.png && systemctl suspend"

# Performance modes
# https://wiki.archlinux.org/title/Lenovo_ThinkPad_X1_Carbon_(Gen_9)#Performance_modes
alias balanced="sudo bash -c 'echo balanced > /sys/firmware/acpi/platform_profile'"
alias performance="sudo bash -c 'echo performance > /sys/firmware/acpi/platform_profile'"


singlehead () {
    cmd="xrandr"
    case $HOSTNAME in
        "zeratul")
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
            ;;
        "hive")
            cmd+=" --output HDMI-A-1 --off"
            cmd+=" --output DVI-D-0 --auto"
            ;;
        *) echo "Unrecognized hostname"; exit 1;;
    esac
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

2head () {
    cmd="xrandr"
    cmd+=" --output HDMI-A-1 --auto --rotate left --left-of DVI-D-0"
    cmd+=" --output DVI-D-0 --auto --primary"
    $cmd
}

3head () {
    # On wayland, it is better to use wdisplays
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

pulseaudio-unfuck() {
    # https://knowledgebase.frame.work/fedora-audio-troubleshooting-guide-BJAe1Kr0o
    systemctl --user restart wireplumber pipewire pipewire-pulse
}

swapflush() {
    sudo swapoff -a
    sudo swapon -a
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


# Add Emacs specific configuration for Vterm.
# Functions vterm_printf and vterm_cmd are copy-pasted from the upstream readme
# https://github.com/akermu/emacs-libvterm
# Then follows my personal configuration
if [[ "$INSIDE_EMACS" = "vterm" ]]; then
    vterm_printf() {
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
            vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
            shift
        done
        vterm_printf "51;E$vterm_elisp"
    }

    # This has higher priority than $PATH so when running `emacs` in vterm,
    # this function will be executed instead of `/usr/bin/emacs`.
    emacs() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }
fi
