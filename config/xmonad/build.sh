#/bin/bash -xeu

exec stack ghc -- --make xmonad.hs -i -ilib -fforce-recomp -main-is main -v0 -o xmonad-x86_64-linux