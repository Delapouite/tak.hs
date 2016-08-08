# tak.hs

CLI version of [Tak (the board game)](http://cheapass.com/node/215) playable in hot-seat mode. Written in Haskell to practice.

Warning: this game's still in development. So you may still stumble upon a few rough edges.

Here's what it looks like:

```
P1's deck: 17 1C
P2's deck: 16 1c

5 . f . . .
4 . f . . .
3 . F F . .
2 . . f F .
1 s F . . .
  a b c d e
```

Fancy isn't it? Let's discover what this beautiful ASCII art means in the following sections.
But first, don't forget to read the [rules](http://cheapass.com/sites/default/files/TakBetaRules3-10-16.pdf) otherwise it won't make any sense.
Also, there's no Artificial Intelligence support yet, so you need to find a local friend ;)

## Install

This project is managed by [Stack](http://docs.haskellstack.org/en/stable/README/):

  - git clone this repository
  - `stack build` to generate the binaries
  - `stack test` to run the HUnit tests (optional)

## Usage

`stack exec tak-exe` to launch the game
`stack exec tak-exe -- --colors` to launch the game with colors
`stack exec tak-exe -- --big`    to launch the game with full stacks displayed in overview

### Size of the board

At the beginning we need to choose the size of the board we want to play:

```sh
Welcome to Tak.hs
Size of the board? [3..8]
```
Let's enter a number between `3` and `8` (both included), and we're ready to go!

### Board view

In the first two lines we see stones counters in each player decks.
`1C` means that 1 *capstone* is available.
Uppercase letters belong to Player 1 and lowercase letters belong to Player 2.

Below, the empty board is displayed (here with `5` size) with coordinates on both axes.

```
P1's deck: 21 1C
P2's deck: 21 1c

5 . . . . .
4 . . . . .
3 . . . . .
2 . . . . .
1 . . . . .
  a b c d e
```

### Prompt

This is the prompt, it displays the current turn and which player should enter a command.

```
turn 1 / P1>
```

### Commands

The game has just started and the board is empty. Let's fill it! It's Player 1's turn.

#### place

To add a stone to the board, use the `place` command with a *valid* `xy` coordinates.

```
turn 1 / P1> place b2

5 . . . . .
4 . . . . .
3 . . . . .
2 . f . . .
1 . . . . .
  a b c d e
```

By default the `place` add a *flat* stone. If we want to precise the type of stone, we just prepend its initial before the coordinates:

```
turn 1 / P2> place sc2

5 . . . . .
4 . . . . .
3 . . . . .
2 . f S . .
1 . . . . .
  a b c d e
```

`sxy` will place a *standing stone*, `cxy` will place a *capstone*.

Currently there are two stones on the board:
  - `b2` is a `f`lat stone, belonging to Player 2.
  - `c2` is a `S`tanding stone, belonging to Player 1.

Remember that in the Tak rules, on the first turn we must place a stone belonging to the opponent.

Let's add a few more:

```
turn 2 / P1> place b4
turn 2 / P2> place d2
turn 3 / P1> place b3
turn 3 / P2> place b1

5 . . . . .
4 . F . . .
3 . F . . .
2 . f S f .
1 . f . . .
  a b c d e
```

We can only place stones from our deck to empty cells. So what's next?

#### move

We want to move some stones for sure! It's Player 1's turn. What about stacking the flat stone in b4 on top of the one in b3?

```
turn 4 / P1> move b4-
```

This `move` command interprets [Portable Tak Notation](https://www.reddit.com/r/Tak/wiki/portable_tak_notation)

To move we must at least indicate our starting point and a direction:

  - `+` means North
  - `-` means South
  - `>` means East
  - `<` means West

So in our case, we started from b4 and went south:

```
5 . . . . .
4 . . . . .
3 . F . . .
2 . f S f .
1 . f . . .
  a b c d e
```

We could precise how many stones we want to take from the starting stack.
So `move b4-` and `move 1b4-` are equivalent.
If you take more than one stone and want to decide how to spread them on the board, append a series of number to your command.
For example `4e5<121` means "take 4 stones from e5 and spread them to the west, leaving 1 stone, then 2 stones and finally 1 stone".
Again, please refer the [PTN docs](https://www.reddit.com/r/Tak/wiki/portable_tak_notation) for more information on this notation.

But wait? It seems like a stone has disappeared from the board! Don't panic, it still here!
The issue we face is that Tak is 3D game and for now we only have a 2D bird view. The letters we see only represent the top stone of each cell.
In this view there are no way to tell the height of a stack on a cell. Introducing the `show` command…

#### show

The `show` command is designed to see the board from the side:

```
turn 4 / P2> show b

    F
. . F f f
5 4 3 2 1
```

All is clearer now. There are two stones on b3. We could also inspect the row to see an alternative view:

```
turn 4 / P2> show 3

  F
. F . . .
a b c d e
```

#### options

During runtime you can enable/disable options:

```
turn 4 / P2> set colors true
turn 4 / P2> set colors false
```

Have a good game! For any questions or suggestions, feel free to open a [GH issue](https://github.com/Delapouite/tak.hs/issues)

