# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`


 DOMAIN: *GAME LIKE SHOP*

This programs desging is the design for a shop that simulates video game shop to buy or sell various items.

### BNF
```
<shop> ::= <command> 
<command> ::= <buy> | <sell> 
<buy> ::=  <item> | <bundle>
<sell> ::= <item>
<item> ::= <item_name> <item_price>
<item_name> ::= "Sword " | "Shield " | "Potion " | "Armor " | "Rune "
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<bundle> ::= "(" <item> " and " <item> ")" | "(" <item> " and " <bundle> ")" | "(" <bundle> " and " <bundle> ")"
<item_price> ::= <number> <currency_type> |  <number> <currency_type> <number> <currency_type> | <number> <currency_type>  <number> <currency_type>  <number> <currency_type>
<currency_type> ::= " gold coins " | " silver coins " | " copper coins "

```

## Command examples:

# Recursion with bundle:

1. `The bundle can be made out of other bundles through recursion`
`this can look something like this: ( <item1> and ((<item2> and <item3) and <item5>))`

2. Concreate example: buy (Shield 88 gold coins 056 gold coins  and ((Armor 6 gold coins 089 silver coins  and Sword 32 gold coins 92 copper coins 7 copper coins ) and (Sword 6 silver coins  and Sword 1 copper coins 712 silver coins )))

# Diffrence between buy and sell:
1. Sell comand is a player selling 1 of their items back to the shop
2. Buy command is a player able to choose an item to buy or to buy a bundle of items
3. simple and common choice is to buy/sell a single item `buy Sword 6 gold coins `