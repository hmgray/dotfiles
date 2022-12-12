#!/bin/sh -e

START_DIR="$(pwd -P)"
BASE_DIR="$(cd "$(dirname "$0")" && pwd)"

cd $BASE_DIR
for i in _*; do
  LINK_NAME="$HOME/"$(echo "$i" | sed s/_/./)""
  if [ -e "$LINK_NAME" ]; then
    mv "$LINK_NAME" "$LINK_NAME.bak"
    echo "Moved $LINK_NAME to $LINK_NAME.bak."
  fi
  ln -s "$BASE_DIR/$i" "$LINK_NAME"
done
cd $START_DIR
