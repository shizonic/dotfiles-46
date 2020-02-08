# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ -z "$INSIDE_EMACS" ] && [ "$(tty)" != /dev/tty1 ] &&
    emacs -nw


backup_root() {
    sudo rsync -aAXv \
         --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/lost+found","/home/*/.local/share/Trash/*","/home/*/.cache/*"} \
         / /mnt/backup
}
