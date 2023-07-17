#! /usr/bin/env bash
#

function lns() {
    if [[ ! -e "${HOME}/$2" ]]; then
        ln -vs "$(pwd)/$1" "${HOME}/$2"
    fi
}

function lncs() {
    lns "config/$1" ".config/$2"
}

function lnhs() {
    lns "home/$1" "$2"
}


lncs "nvim" "nvim"
lncs "xmonad" "xmonad"
lnhs "wezterm.lua" ".wezterm.lua"
lnhs "xmobarrc" ".xmobarrc"
