#!/bin/sh

echo "Installing addons for vs code!"
echo ""

filename='addons.list'
IFS=$'\n'

while IFS= read -r line; do
  code-insiders --install-extensions $temp
done < $filename

echo ""
echo "DONE!"
