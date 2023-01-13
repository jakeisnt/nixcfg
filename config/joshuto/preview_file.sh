#!/usr/bin/env bash

test -z "$joshuto_wrap_id" && exit 1;

path="$1"       # Full path of the previewed file
x="$2"          # x coordinate of upper left cell of preview area
# y="$3"          # y coordinate of upper left cell of preview area
width="$4"      # Width of the preview pane (number of fitting characters)
height="$5"     # Height of the preview pane (number of fitting characters)
let y="$height"/5

# Find out mimetype and extension
mimetype=$(file --mime-type -Lb "$path")
extension=$(/bin/echo "${path##*.}" | awk '{print tolower($0)}')

case "$mimetype" in
    image/png | image/jpeg)
        show_image "$path" $x $y $width $height
        ;;
    video/*)
        ffmpegthumbnailer -i "$path" -f -m -c png -s 0 -o /tmp/vid.png 2>/dev/null
        show_image /tmp/vid.png $x $y $width $height
        sleep 5 && rm -f /tmp/vid.png
        ;;
    *)
        remove_image
esac
