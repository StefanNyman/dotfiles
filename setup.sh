#! /usr/bin/env bash
#

function lns() {
    ln -vs "$(pwd)/$1" "${HOME}/$2"
}

function lncs() {
    lns "config/$1" ".config/$2"
}

function lnhs() {
    lns "home/$1" "$2"
}


lncs "nvim" "nvim"
lnhs "wezterm.lua" ".wezterm.lua"
