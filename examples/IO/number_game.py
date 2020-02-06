#/usr/bin/env python3
import random

def getGuess():
    try:
        print("Type your guess: ")
        guess = int(input())
        if guess > 0 and guess <= 100:
            return guess
        else:
            print("Number needs to be from 1 to 100")
            return getGuess()
    except:
        print("Come again?")
        return getGuess()

def numberGame():
    guesses = 7
    number = random.randrange(1,100)
    print("I'm thinking of a number from 1 to 100. You have 6 guesses.")
    while guesses > 0:
        print("You have " + str(guesses) + " left.")
        guesses = guesses - 1

        guess = getGuess()
        if guess == number:
            print("Correct. You win!")
            return
        elif guess > number:
            print("Too high. Try again!")
        else:
            print("Too low. Try again!")
    print("You lose! The number was " + str(number) + ".")

while True:
    numberGame()
    print("Do you want to play again? (y/n)")
    answer = input()
    if answer == "n":
        break
    elif answer == "y":
        pass
    else:
        print("I'll take that as a yes!")
