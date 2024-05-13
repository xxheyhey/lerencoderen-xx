#!/bin/bash

x=1

while [[ $x -le 30 ]]
do
  read -p "Pushup $x: Press enter to continue."
  (( x ++ ))
done

echo "Good job!"
