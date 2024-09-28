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
<buy> ::=  <item>
<sell> ::= <item>
<item> ::= <item_name> <item_price>
<item_name> ::= "sword " | "shield " | "potion " | "armor " | "rune "
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<bundle> ::= <item> <item> | <item> <bundle> | <bundle> <bundle>
<item_price> ::= <number> <currency_type> |  <number> <currency_type> <number> <currency_type> | <number> <currency_type>  <number> <currency_type>  <number> <currency_type>
<currency_type> ::= " gold coins " | " silver coins " | " copper coins "

```
