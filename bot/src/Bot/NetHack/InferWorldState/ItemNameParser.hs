{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Bot.NetHack.InferWorldState.ItemNameParser
  ( nameToItem )
  where

import Bot.NetHack.WorldState
import Data.List
import qualified Data.Text as T
import Text.Regex.TDFA hiding ( match )

nameToItem :: T.Text -> Item
nameToItem txt | T.isInfixOf "statue of" txt = Item
  { _itemIdentity = Statue
  , _corrosion = 0
  , _enchantment = Nothing
  , _quantity = 1
  , _buc = Nothing
  , _itemAppearance = T.strip txt }
nameToItem (T.strip -> txt'') = Item
  { _itemIdentity = item_identity
  , _corrosion = corrosion
  , _enchantment = enchantment
  , _quantity = quantity
  , _buc = buc
  , _itemAppearance = T.pack name }
 where
  txt' = T.unpack txt''


  item_identity | T.isSuffixOf " corpse" (T.pack name) =
    Corpse (T.dropEnd 7 $ T.pack name)

                | otherwise = case name of

    "food ration" -> Food
    "food rations" -> Food
    "gunyoki" -> Food
    "slime mold" -> Food
    "slime molds" -> Food
    "lembas wafer" -> Food
    "lembas wafers" -> Food
    "lichen corpse" -> Food
    "lichen corpses" -> Food
    "K-ration" -> Food
    "K-rations" -> Food
    "C-ration" -> Food
    "C-rations" -> Food
    "cream pie" -> Food
    "cream pies" -> Food
    "candy bar" -> Food
    "candy bars" -> Food
    "pancake" -> Food
    "pancakes" -> Food
    "melon" -> Food
    "melons" -> Food

    "Grayswandir" -> Weapon 25
    "Excalibur" -> Weapon 20
    "long sword" -> Weapon 10
    "katana" -> Weapon 11
    
    "magic marker" -> MagicMarker
    "pick-axe" -> PickAxe

    "skeleton key" -> Key
    "key" -> Key
    "lock pick" -> Key
    "osaku" -> Key
    "credit card" -> CreditCard

    "dwarvish mithril-coat" -> armor 6 Body
    "elven mithril-coat" -> armor 5 Body
    "orcish helm" -> armor 1 Helmet
    "iron skull cap" -> armor 1 Helmet
    "splint mail" -> armor 6 Body
    "gray dragon scale mail" -> Armor 9 Body MagicResistance
    "gray dragon scales" -> Armor 3 Body MagicResistance
    "silver dragon scale mail" -> Armor 9 Body Reflection
    "silver dragon scales" -> Armor 3 Body Reflection
    txt | isInfixOf "dragon scale mail" txt -> armor 9 Body
    txt | isInfixOf "dragon scales" txt -> armor 3 Body
    "coarse mantelet" -> armor 0 Cloak
    "orcish cloak" -> armor 0 Cloak
    "dwarvish cloak" -> armor 0 Cloak
    "hooded cloak" -> armor 0 Cloak
    "leather cloak" -> armor 1 Cloak
    "slippery cloak" -> armor 1 Cloak
    "oilskin cloak" -> armor 1 Cloak
    "elven cloak" -> armor 1 Cloak
    "faded pall" -> armor 1 Cloak
    "alchemy smock" -> armor 1 Cloak
    "apron" -> armor 1 Cloak
    "fedora" -> armor 0 Helmet
    "dented pot" -> armor 1 Helmet
    "elven leather helm" -> armor 1 Helmet
    "leather hat" -> armor 1 Helmet
    "hard hat" -> armor 2 Helmet
    "dwarvish iron helm" -> armor 2 Helmet
    "leather gloves" -> armor 1 Gloves
    "yugake" -> armor 1 Gloves
    "small shield" -> armor 1 Shield
    "orcish shield" -> armor 1 Shield
    "red-eyed shield" -> armor 1 Shield
    "Uruk-hai shield" -> armor 1 Shield
    "white-handed shield" -> armor 1 Shield
    "large round shield" -> armor 2 Shield
    "dwarvish roundshield" -> armor 2 Shield
    "elven shield" -> armor 2 Shield
    "shield of reflection" -> Armor 2 Shield Reflection
    "polished silver shield" -> Armor 2 Shield Reflection
    "low boots" -> armor 1 Shoes
    "walking shoes" -> armor 1 Shoes
    "high boots" -> armor 2 Shoes
    "jackboots" -> armor 2 Shoes
    "iron shoes" -> armor 2 Shoes
    "hard shoes" -> armor 2 Shoes

    "leather drum" -> NonTonalInstrument
    "drum of earthquake" -> NonTonalInstrument
    "drum" -> NonTonalInstrument

    "tooled horn" -> ScaryTonalInstrument

    "boulder" -> Boulder
    "boulders" -> Boulder

    _ -> StrangeItem

  txt_article_removed = if isPrefixOf "a " txt'
                          then drop 2 txt'
                          else (if isPrefixOf "an " txt'
                                  then drop 3 txt'
                                  else (if isPrefixOf "the " txt'
                                          then drop 4 txt'
                                          else txt'))

  (quantity, txt_quantity_removed) = case txt_article_removed =~ ("([0-9]+) (.+)" :: String) of
    [[_whole, quantity, rest]] -> (read quantity, rest)

    _ -> (1, txt_article_removed)

  (buc, txt_buc_removed) = if isPrefixOf "uncursed " txt_quantity_removed
                             then (Just Uncursed, drop 9 txt_quantity_removed)
                             else (if isPrefixOf "blessed " txt_quantity_removed
                                     then (Just Blessed, drop 8 txt_quantity_removed)
                                     else (if isPrefixOf "cursed " txt_quantity_removed
                                             then (Just Cursed, drop 7 txt_quantity_removed)
                                             else (Nothing, txt_quantity_removed)))

  (corrosion, txt_corrosion_removed) = foldDamage txt_buc_removed

  foldDamage src_text = case src_text =~
     ("^(corroded|very corroded|thoroughly corroded|rusted|very rusted|thoroughly rusted|rotted|very rotted|thoroughly rotted|burnt|very burnt|thoroughly burnt) (.+)$" :: String) of
      [[_whole, damage, rest]] ->
        let damage_level = if | isInfixOf "very" damage -> 2
                              | isInfixOf "thoroughly" damage -> 3
                              | otherwise -> 1
            (rest_damage, rest_txt) = foldDamage rest
         in (max damage_level rest_damage, rest_txt)

      _ -> (0, src_text)

  (enchantment, enchantment_removed) = case txt_corrosion_removed =~ ("(\\+|\\-)([0-9]+) (.+)" :: String) of
    [[_whole, sign, enchantment, rest]] ->
      (if sign == "-"
         then Just $ negate $ read enchantment
         else Just $ read enchantment
      ,rest)

    _ -> (Nothing, txt_buc_removed)

  -- foodstuff
  (_is_partly_eaten, partlyeaten_removed) = if isPrefixOf "partly eaten " enchantment_removed
    then (True, drop 13 enchantment_removed)
    else (False, enchantment_removed)

  -- wizard mode
  weight_removed = case partlyeaten_removed =~ ("(.+) \\([0-9]+ aum\\)" :: String) of
    [[_whole, first_part]] -> first_part
    _ -> partlyeaten_removed

  name = weight_removed

