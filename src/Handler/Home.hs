{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import

import qualified Data.Set as Set
import Yesod.Table hiding (maybe, show)
import qualified Text.Blaze as Blaze

import Database.Persist.Sql

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
        setUltDestCurrent
        $(widgetFile "spell-index")

spellTable :: [Entity Spell] -> Widget
spellTable = buildBootstrap $ mconcat
    [ linked "Name" (spellName . entityVal) (SpellR . entityKey)
    , text "Level" (spellLevel . entityVal)
    , text "School" (spellSchool . entityVal)
    , text "Descriptor" (spellDescriptor . entityVal)
    , text "Spell Resistance" (spellSpellResistance . entityVal)
    , text "Reference" (spellReference . entityVal)
    , widget "Toggle Spell" (\(Entity spellId _) ->
        [whamlet|
            <button data-spell-id=#{tshow (fromSqlKey spellId)} .btn-large .toggle-button>
                Toggle
        |]
        )
    ]

getSpellR :: SpellId -> Handler Html
getSpellR spellId = do
    Spell{..} <- runDB $ get404 spellId
    defaultLayout $ do
        setTitle $ do
            "Spellcards - "
            Blaze.text spellName
        $(widgetFile "spell-show")

getSpellListR :: Handler Html
getSpellListR = do
    spellList <- readSpellList
    defaultLayout $ [whamlet|
        #{tshow spellList}
    |]

putUpdateSpellListR :: SpellId -> Handler ()
putUpdateSpellListR spellId = do
    addSpell spellId
    redirectUltDest HomeR

deleteUpdateSpellListR :: SpellId -> Handler ()
deleteUpdateSpellListR spellId = do
    removeSpell spellId
    redirectUltDest HomeR

addSpell :: SpellId -> Handler ()
addSpell spellId = do
    spellList <- readSpellList
    writeList (Set.insert spellId spellList)

removeSpell :: SpellId -> Handler ()
removeSpell spellId = do
    spellList <- readSpellList
    writeList (Set.delete spellId spellList)

readSpellList :: Handler (Set SpellId)
readSpellList = do
    mlist <- lookupSession "spell-list"
    case mlist >>= readMay of
        Nothing -> do
            writeList mempty
            pure mempty
        Just list ->
            pure (Set.fromList (map toSqlKey list))

writeList :: Set SpellId -> Handler ()
writeList = setSession "spell-list" . pack . show . map fromSqlKey . Set.toList
