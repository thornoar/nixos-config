brightfile=""
if [ "$PCTYPE" = "laptop" ]; then
    brightfile="/sys/class/backlight/intel_backlight/brightness"
    bc <<< "$1 + $(<$brightfile)" > "$brightfile"
fi
