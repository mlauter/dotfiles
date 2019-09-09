function prettydiff {
    colordiff -y <(cat $1 | sort | uniq) <(cat $2 | sort | uniq) | less -Sn
}
