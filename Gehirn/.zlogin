if which tmux > /dev/null 2>&1; then
    if [ -z "$TMUX" ]; then
        tmux list-session >/dev/null 2>&1| wc -l| awk '{print $0}'| read count

        if $(tmux has-session >/dev/null 2>&1); then
            echo -e "1\tAttach to current session"
            echo -e "2\tCreate new session"
            echo -e "3\tWithout tmux"
            echo -n "Which whould you like? (default:1): "
            read choose

            case "$choose" in
                "3" )
                    ;;
                "2" )
                    exec tmux new-session
                    ;;
                * )
                    if [ $count -eq 1 ]; then
                        exec tmux attach-session
                    else
                        tmux list-session
                        echo -n "Which session whould you like to attach? "
                        read choose
                        exec tmux attach-session -t $choose
                    fi
                    ;;
            esac
        else
            echo -e "1\tCreate new session"
            echo -e "2\tWithout tmux"
            echo -n "Which whould you like? (default:1): "
            read choose

            case "$choose" in
                "2" )
                    ;;
                * )
                    exec tmux new-session
                    ;;
            esac
        fi
    fi
fi
