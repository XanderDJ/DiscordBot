module Pokemon.Nature where

import Pokemon.Types
    ( Stat(SPATK, NEUTRAL, ATK, DEF, SPEED, SPDEF), Nature(..) )

hardy :: Nature
hardy = Nature "Hardy" NEUTRAL NEUTRAL

docile :: Nature
docile = Nature "Docile" NEUTRAL NEUTRAL

serious :: Nature
serious = Nature "Serious" NEUTRAL NEUTRAL

bashful :: Nature
bashful = Nature "Bashful" NEUTRAL NEUTRAL

quirky :: Nature
quirky = Nature "Quirky" NEUTRAL NEUTRAL

lonely :: Nature
lonely = Nature "Lonely" ATK DEF

brave :: Nature
brave = Nature "Brave" ATK SPEED

adamant :: Nature
adamant = Nature "Adamant" ATK SPATK

naughty :: Nature
naughty = Nature "Naughty" ATK SPDEF

bold :: Nature
bold = Nature "Bold" DEF ATK

relaxed :: Nature
relaxed = Nature "Relaxed" DEF SPEED

impish :: Nature
impish = Nature "Impish" DEF SPATK

lax :: Nature
lax = Nature "Lax" DEF SPDEF

timid :: Nature
timid = Nature "Timid" SPEED ATK

hasty :: Nature
hasty = Nature "Hasty" SPEED DEF

jolly :: Nature
jolly = Nature "Jolly" SPEED SPATK

naïve :: Nature
naïve = Nature "Naïve" SPEED SPDEF

modest :: Nature
modest = Nature "Modest" SPATK ATK

mild :: Nature
mild = Nature "Mild" SPATK DEF

quiet :: Nature
quiet = Nature "Quiet" SPATK SPEED

rash :: Nature
rash = Nature "Rash" SPATK SPDEF

calm :: Nature
calm = Nature "Calm" SPDEF ATK

gentle :: Nature
gentle = Nature "Gentle" SPDEF DEF

sassy :: Nature
sassy = Nature "Sassy" SPDEF SPEED

careful :: Nature
careful = Nature "Careful" SPDEF SPATK

getNature :: String -> Maybe Nature
getNature "hardy" = Just hardy
getNature "docile" = Just docile
getNature "serious" = Just serious
getNature "bashful" = Just bashful
getNature "quirky" = Just quirky
getNature "lonely" = Just lonely
getNature "brave" = Just brave
getNature "adamant" = Just adamant
getNature "naughty" = Just naughty
getNature "bold" = Just bold
getNature "relaxed" = Just relaxed
getNature "impish" = Just impish
getNature "lax" = Just lax
getNature "timid" = Just timid
getNature "jolly" = Just jolly
getNature "hasty" = Just hasty
getNature "naïve" = Just naïve
getNature "naive" = Just naïve
getNature "modest" = Just modest
getNature "mild" = Just mild
getNature "quiet" = Just quiet
getNature "rash" = Just rash
getNature "calm" = Just calm
getNature "gentle" = Just gentle
getNature "sassy" = Just sassy
getNature "careful" = Just careful
getNature _ = Nothing