#!/bin/sh

DMENU_OPTIONS="-l 10"
DMENU_COLORS="-nb #000033 -nf #ccccff -sb #0000ff -sf #ffff00"
BOOKMARKS_FILE="$XDG_CONFIG_HOME/hbro/bookmarks"
BROWSER="hbro"
SCRIPT_PATH="$0"
ACTION="$1"


case $ACTION in
    # Add current page to bookmarks
    "add" )
        # Not enough arguments
        if [ $# -lt 3 ]; then
            echo "Not enough argument."
            exit 1
        fi

        URI="$2"
        TAGS=$(echo $* | cut -d' ' -f3-)

        echo -e "$URI $TAGS" >> $BOOKMARKS_FILE
    ;;

    # Add all currently opened pages to bookmarks
    "add-all" )
        # Not enough arguments
        if [ $# -lt 3 ]; then
            echo "Not enough argument."
            exit 1
        fi

        SOCKET_DIR="$2"
        TAGS=$(echo $* | cut -d' ' -f3-)

        for PID in `pidof hbro`; do
            SOCKET="$SOCKET_DIR/hbro.$PID"
            URI=`zmqat -req "ipc://$SOCKET" "getUri"`
            echo -e "$URI $TAGS" >> $BOOKMARKS_FILE
        done
    ;;

    # Delete all bookmarks with given tag
    "delete-tag" )
        TAG=$(awk '{for (i = 2; i <= NF; i++) print $i; }' "$BOOKMARKS_FILE" | sort -bfu | dmenu $DMENU_OPTIONS $DMENU_COLORS -p "Delete bookmarks tag:")

        if [ -n "$TAG" ]; then
            sed -i".bak" '/ '$TAG'/d' "$BOOKMARKS_FILE"
        fi
    ;;

    # Load a single bookmark
    "load" )
        # Not enough arguments
        if [ $# -lt 2 ]; then
            echo "Not enough argument."
            exit 1
        fi

        SOCKET=$2
        URI=$(sed -e 's/%/%%/g' "$BOOKMARKS_FILE" | awk '{for (i = 2; i <= NF; i++) printf "[%s] ", $i; printf $1 "\n"}' | sort -bf | dmenu $DMENU_OPTIONS $DMENU_COLORS -p "Load bookmark:" | awk '{print $NF}')

        [ -n "$URI" ] && zmqat -req "ipc://$SOCKET" "loadUri $URI"
    ;;

    # Load all bookmarks for a given tag
    "load-tag" )
        TAG=$(awk '{for (i = 2; i <= NF; i++) print $i; }' "$BOOKMARKS_FILE" | sort -bfu | dmenu $DMENU_OPTIONS $DMENU_COLORS -p "Load bookmarks with tag:")
        URIs=$(awk '/'$TAG'/ {print $1}' "$BOOKMARKS_FILE" | sort -bfu)

        [ -n "$TAG" ] && [ -n "$URIs" ] || exit 2

        for URI in $URIs; do
            nohup $BROWSER -u "$URI" &
        done
    ;;

    * )
        echo "Bookmarks manager: bad action"
        echo "Usage: bookmarks.sh [COMMAND]"
        echo ""
        echo "where commands are:"
        echo " add          - Add a new single bookmark."
        echo " add-all      - Add all currently opened pages as bookmarks."
        echo " delete-tag   - Delete all bookmarks with a given tag."
        echo " load         - Load a single bookmark in current $BROWSER instance."
        echo " load-tag     - Load all bookmarks with a given tag in new $BROWSER instances."
    ;;
esac
