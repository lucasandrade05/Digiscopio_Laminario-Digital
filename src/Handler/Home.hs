{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql
import Handler.Pesq
import Handler.Login


getHomeR :: Handler Html
getHomeR = do
    userlogado <- lookupSession "_ID"
    let raiz = toSqlKey(1)
    (widget3, enctype) <- generateFormPost (formCadUser raiz False)
    defaultLayout $ do 
        setTitle "Digiscópio - Home - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/home.hamlet")
        
    
getExplorarR :: Int -> Handler Html
getExplorarR page = do
    userlogado <- lookupSession "_ID"
    contagem <- runDB $ count[LaminaId >. toSqlKey(0)]
    laminas <- runDB $ selectList [][Desc LaminaId, LimitTo 8, OffsetBy $ (page - 1) * 8]
    let primeirores = (page * 8) - 7
    let ultimapag = ((contagem :: Int) `div` 8 + 1)
    let paginas
            | ultimapag == 1 =  [-1]
            | page==ultimapag = [page-5..ultimapag]
            | ultimapag > 5 && page == 1 = ([page..page+4] ++ [ultimapag])
            | ultimapag > 5 && page == 2 = ([page-1..page+3] ++ [ultimapag])
            | page+1 == ultimapag = [page-4..ultimapag]
            | page+2 == ultimapag = [page-3..ultimapag]
            | page+2 < ultimapag = ([page-2..page+2] ++ [ultimapag])
            | otherwise = ([page-4..ultimapag])
    let ultimores
            | page==ultimapag = contagem
            | otherwise = page * 8
    defaultLayout $ do 
        setTitle "Digiscópio - Explorar - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/explorar.hamlet") 
        


