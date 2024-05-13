#!/bin/bash

echo "Welcome tarnished. Please select your starting class:
1 - Samurai
2 - Magician
3 - Prophet
4 - Deprived"

read class

case $class in

	1)
		type="Samurai"
		hp=11
		attack=14
		magic=6
		;;
	2)
		type="Samurai"
                hp=10
                attack=8
                magic=15
		;;
	3)
		type="Prophet"
                hp=14
                attack=11
                magic=12
                ;;
	4)
		type="Deprived"
                hp=1
                attack=1
                magic=1
                ;;
esac

echo "You have chosen the $type class. These are your stats:"
echo "HP: $hp"
echo "Attack: $attack"
echo "Magic: $magic"

sleep 2

echo "You Died"
sleep 1


#First beast battle


echo "A beast approaches."

echo "You will fight the beast with the power of a number."

echo "Pick a number. (0/1)"

read number


echo "Fighting..."


if [[ $number == $((($RANDOM%2))) && 30 > 17 || $number == "pilskes" ]];then
	echo "Beast Vanquished"
else
	echo "You Died"
	exit 1
fi


#Boss battle: Margit the Fell

echo "You approach Margit the Fell"
echo "You decide to battle Margit"
echo "Pick a number. (0,9)"

read number

if [[ $number == $((($RANDOM % 10))) || $number == "pilskes" ]];then
	echo "Great Foe Vanquished"
elif [[ $USER == "root" ]]; then
	echo "root always wins. Great Foe Vanquished"
else
	echo "You Died"
	exit 1
fi
