Adeonbot
--------

NetHack playing bot.

Build instructions
------------------

Get `stack` binary in PATH. You can look inside
http://docs.haskellstack.org/en/stable/README/ for some instructions.

Then build:

    stack setup      # If you don't have appropriate GHC
    stack build
    stack exec adeonbot -- botconfig.yaml

That config file, `botconfig.yaml` should have the NetHack command to run.
Here's an example file contents I use on my local setup:

    name: Adeonbot
    password: supersecret_password             # for public servers
    latency: 1000000000                        # nanoseconds to wait until terminal gets idle
    nethackCommand: ["./nh360/games/nethack"]

.nethackrc
----------

    OPTIONS=color,!autodig,!autopickup
    OPTIONS=boulder:0
    OPTIONS=!sparkle
    OPTIONS=hilite_pet
    OPTIONS=!hilite_pile
    OPTIONS=lit_corridor
    OPTIONS=!rest_on_space
    OPTIONS=time
    OPTIONS=!showscore
    OPTIONS=!showexp
    OPTIONS=showrace
    OPTIONS=!pet
    OPTIONS=!timed_delay
    OPTIONS=number_pad:0

    SYMBOLS=S_vcdoor:7
    SYMBOLS=S_hcdoor:7
    SYMBOLS=S_grave:7
    SYMBOLS=S_rock:8
    SYMBOLS=S_ghost:X


