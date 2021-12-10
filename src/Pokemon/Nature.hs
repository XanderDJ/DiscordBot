module Pokemon.Nature where

import Pokemon.Types
    ( Stat(SPA, NEU, ATK, DEF, SPE, SPD), Nature(..) )
import Data.Text

hardy :: Nature
hardy = Nature "Hardy" NEU NEU

docile :: Nature
docile = Nature "Docile" NEU NEU

serious :: Nature
serious = Nature "Serious" NEU NEU

bashful :: Nature
bashful = Nature "Bashful" NEU NEU

quirky :: Nature
quirky = Nature "Quirky" NEU NEU

lonely :: Nature
lonely = Nature "Lonely" ATK DEF

brave :: Nature
brave = Nature "Brave" ATK SPE

adamant :: Nature
adamant = Nature "Adamant" ATK SPA

naughty :: Nature
naughty = Nature "Naughty" ATK SPD

bold :: Nature
bold = Nature "Bold" DEF ATK

relaxed :: Nature
relaxed = Nature "Relaxed" DEF SPE

impish :: Nature
impish = Nature "Impish" DEF SPA

lax :: Nature
lax = Nature "Lax" DEF SPD

timid :: Nature
timid = Nature "Timid" SPE ATK

hasty :: Nature
hasty = Nature "Hasty" SPE DEF

jolly :: Nature
jolly = Nature "Jolly" SPE SPA

naïve :: Nature
naïve = Nature "Naïve" SPE SPD

modest :: Nature
modest = Nature "Modest" SPA ATK

mild :: Nature
mild = Nature "Mild" SPA DEF

quiet :: Nature
quiet = Nature "Quiet" SPA SPE

rash :: Nature
rash = Nature "Rash" SPA SPD

calm :: Nature
calm = Nature "Calm" SPD ATK

gentle :: Nature
gentle = Nature "Gentle" SPD DEF

sassy :: Nature
sassy = Nature "Sassy" SPD SPE

careful :: Nature
careful = Nature "Careful" SPD SPA

getNature :: Text -> Maybe Nature
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