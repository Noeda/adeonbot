{-# LANGUAGE OverloadedStrings #-}

module Bot.NetHack.EdibleCorpses
  ( isEdible )
  where

import qualified Data.Text as T

isEdible :: T.Text -> Bool
isEdible name = isEdible' (stripPartiallyEaten $ T.strip $ T.toLower name)

stripPartiallyEaten :: T.Text -> T.Text
stripPartiallyEaten txt =
  if T.isInfixOf "partially eaten " txt
    then T.drop 16 txt
    else txt

isEdible' :: T.Text -> Bool
isEdible' "newt" = True
isEdible' "lichen" = True
isEdible' "jackal" = True
isEdible' "coyote" = True
isEdible' "owlbear" = True
isEdible' "floating eye" = True
isEdible' "giant ant" = True
isEdible' "fire ant" = True
isEdible' "black unicorn" = True
isEdible' "white unicorn" = True
isEdible' "gray unicorn" = True
isEdible' "pony" = True
isEdible' "horse" = True
isEdible' "warhorse" = True
isEdible' "leocrotta" = True
isEdible' "quivering blob" = True
isEdible' "gelatinous cube" = True
isEdible' "pyrolisk" = True
isEdible' "fox" = True
isEdible' "wolf" = True
isEdible' "warg" = True
isEdible' "rothe" = True
isEdible' "winter wolf" = True
isEdible' "winter wolf cub" = True
isEdible' "hell hound" = True
isEdible' "hell hound pup" = True
isEdible' "panther" = True
isEdible' "jaguar" = True
isEdible' "tiger" = True
isEdible' "gargoyle" = True
isEdible' "winged gargoyle" = True
isEdible' "mind flayer" = True
isEdible' "master mind flayer" = True
isEdible' "imp" = True
isEdible' "goblin" = True
isEdible' "hobgoblin" = True
isEdible' "orc" = True
isEdible' "hill orc" = True
isEdible' "mordor orc" = True
isEdible' "uruk-hai" = True
isEdible' "orc shaman" = True
isEdible' "orc-captain" = True
isEdible' "rock piercer" = True
isEdible' "iron piercer" = True
isEdible' "glass piercer" = True
isEdible' "mumak" = True
isEdible' "wumpus" = True
isEdible' "titanothere" = True
isEdible' "baluchitherium" = True
isEdible' "cave spider" = True
isEdible' "black naga hatchling" = True
isEdible' "blue naga hatchling" = True
isEdible' "golden naga hatchling" = True
isEdible' "guardian naga hatchling" = True
isEdible' "red naga hatchling" = True
isEdible' "black naga" = True
isEdible' "blue naga" = True
isEdible' "golden naga" = True
isEdible' "guardian naga" = True
isEdible' "red naga" = True
isEdible' "ape" = True
isEdible' "yeti" = True
isEdible' "mastodon" = True
isEdible' "monkey" = True
isEdible' "sasquatch" = True
isEdible' "sewer rat" = True
isEdible' "giant rat" = True
isEdible' "rock mole" = True
isEdible' "woodchuck" = True
isEdible' "baby long worm" = True
isEdible' "baby purple worm" = True
isEdible' "long worm" = True
isEdible' "purple worm" = True
isEdible' "zruty" = True
isEdible' "raven" = True
isEdible' "shrieker" = True
isEdible' "red mold" = True
isEdible' "brown mold" = True
isEdible' "stone giant" = True
isEdible' "hill giant" = True
isEdible' "fire giant" = True
isEdible' "frost giant" = True
isEdible' "storm giant" = True
isEdible' "ettin" = True
isEdible' "minotaur" = True
isEdible' "jabberwock" = True
isEdible' "ogre" = True
isEdible' "ogre lord" = True
isEdible' "ogre king" = True
isEdible' "gray ooze" = True
isEdible' "rust monster" = True
isEdible' "disenchanter" = True
isEdible' "garter snake" = True
isEdible' "troll" = True
isEdible' "ice troll" = True
isEdible' "rock troll" = True
isEdible' "water troll" = True
isEdible' "olog-hai" = True
isEdible' "umber hulk" = True
isEdible' "wraith" = True
isEdible' "carnivorous ape" = True
isEdible' "flesh golem" = True
isEdible' "gecko" = True
isEdible' "iguana" = True
isEdible' "baby crocodile" = True
isEdible' "crocodile" = True
isEdible' "baby gray dragon" = True
isEdible' "baby silver dragon" = True
isEdible' "baby red dragon" = True
isEdible' "baby white dragon" = True
isEdible' "baby orange dragon" = True
isEdible' "baby black dragon" = True
isEdible' "baby blue dragon" = True
isEdible' "baby green dragon" = True
isEdible' "baby yellow dragon" = True
isEdible' "gray dragon" = True
isEdible' "silver dragon" = True
isEdible' "red dragon" = True
isEdible' "white dragon" = True
isEdible' "orange dragon" = True
isEdible' "black dragon" = True
isEdible' "blue dragon" = True
isEdible' "green dragon" = True
isEdible' "yellow dragon" = True
isEdible' _ = False

