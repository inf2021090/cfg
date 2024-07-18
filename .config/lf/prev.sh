#!/bin/bash

set -euo pipefail

# The coordinates and size for the image preview
X=0
Y=0
WIDTH=60
HEIGHT=20

# Function to handle image previews
preview_image() {
  local file="$1"

  ueberzugpp layer --parser bash -c "
    add image \
      --coords $X,$Y \
      --size $WIDTH,$HEIGHT \
      --path '$file'
  "
}

# Function to handle text previews
preview_text() {
  local file="$1"
  bat --paging=never --style=plain "$file"
}

# Function to determine file type and preview accordingly
handle_file() {
  local file="$1"
  local mimetype=$(file --mime-type -Lb "$file")

  case "$mimetype" in
    image/*)
      preview_image "$file"
      ;;
    text/* | application/json)
      preview_text "$file"
      ;;
    *)
      echo "No preview available for this file type."
      ;;
  esac
}

# Run the file handler
handle_file "$1"

