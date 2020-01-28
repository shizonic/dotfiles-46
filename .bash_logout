# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress
[ -z "$DISPLAY" ] && [ "$(who | grep adam | wc -l)" = 1 ] && {
    fscrypt lock $HOME/LOCKER || bash
}
