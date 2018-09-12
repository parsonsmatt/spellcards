{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

import Yesod.Table hiding (maybe)
import qualified Text.Blaze as Blaze

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Spellcards"
        $(widgetFile "homepage")

getSpellIndexR :: Handler Html
getSpellIndexR = do
    spells <- runDB $ selectList [] []
    let spellList = spellTable spells
    defaultLayout $ do
        setTitle "Spellcards"
        $(widgetFile "spell-index")

spellTable :: [Entity Spell] -> Widget
spellTable = buildBootstrap $ mconcat
    [ linked "Name" (spellName . entityVal) (SpellR . entityKey)
    , text "Level" (spellLevel . entityVal)
    , text "School" (spellSchool . entityVal)
    , text "Descriptor" (spellDescriptor . entityVal)
    , text "Spell Resistance" (spellSpellResistance . entityVal)
    , text "Reference" (spellReference . entityVal)
    ]

getSpellR :: SpellId -> Handler Html
getSpellR spellId = do
    Spell{..} <- runDB $ get404 spellId
    defaultLayout $ do
        setTitle $ do
            "Spellcards - "
            Blaze.text spellName
        $(widgetFile "spell-show")
