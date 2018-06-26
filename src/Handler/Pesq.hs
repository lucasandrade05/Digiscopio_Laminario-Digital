{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Pesq where

import Import
import Database.Persist.Postgresql

data Pesquisa = Pesquisa
    { pesquisa          :: Text
    }
    
formPesquisa :: Form Pesquisa
formPesquisa = renderBootstrap $ Pesquisa
        <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Titulo",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Digite sua pesquisa"),("style","width:70%")]} Nothing
    
toTexto :: Pesquisa -> Text
toTexto (Pesquisa x) = x

postPesqR :: Handler Html
postPesqR = do 
    texto <- runInputPost $ ireq textField "pesquisa"
    opc <- runInputPost $ ireq textField "opc"
    case opc of
        "laminas" -> redirect (BuscarLaminaR texto 1)
        "usuarios" -> redirect (BuscarUserR texto 1)
    

getBuscarLaminaR :: Text -> Int -> Handler Html
getBuscarLaminaR txt page = do
    userlogado <- lookupSession "_ID"
    (widget2, enctype2) <- generateFormPost formPesquisa
    contagem <- runDB $ count[Filter LaminaTitulo (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")]
    verifica <- runDB $ selectFirst[Filter LaminaTitulo (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")][]
    texto <- runDB $ selectList[Filter LaminaTitulo (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")][LimitTo 8, OffsetBy $ (page - 1) * 8]
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
        let nomePagina = "Resultados da busca:"  :: Text
        setTitle "Digiscópio - Resultados da Busca - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/buscalamina.hamlet") 

getBuscarUserR :: Text -> Int -> Handler Html
getBuscarUserR txt page = do
    userlogado <- lookupSession "_ID"
    (widget2, enctype2) <- generateFormPost formPesquisa
    contagem <- runDB $ count([Filter UsuarioLogin (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")]
                          ||.[Filter UsuarioNome (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")])
    verifica <- runDB $ selectFirst([Filter UsuarioLogin (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")]
                          ||.[Filter UsuarioNome (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")])[]
    texto <- runDB $ selectList([Filter UsuarioLogin (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")]
                          ||.[Filter UsuarioNome (Left $ "%"++ txt ++"%") (BackendSpecificFilter "ILIKE")])[LimitTo 8, OffsetBy $ (page - 1) * 8]
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
        let nomePagina = "Resultados da busca:"  :: Text
        setTitle "Digiscópio - Resultados da Busca - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/buscauser.hamlet") 
        
        
userByLamina :: UsuarioId -> Widget
userByLamina usuid = do 
    user <- handlerToWidget $ runDB $ selectFirst [UsuarioId ==. usuid][]
    [whamlet|
     $forall (Entity pid usuario) <- user
       #{usuarioLogin usuario}
      |]
      
fotoByLamina :: LaminaId -> Widget
fotoByLamina lamid = do 
            zooms <- handlerToWidget $ runDB $ selectFirst [ZoomLamina ==. lamid][]
            case zooms of 
                Just (Entity zid zoom) -> do
                    imgzoom <- handlerToWidget $ runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
                    case imgzoom of
                        Just (Entity imgid img) -> do
                            [whamlet|
                                <img src="/static/zoom/#{imgzoomNmimg img}"  width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                            |]
                        Nothing -> do
                            [whamlet|
                                <img src="/static/imgnotfound.png" width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                            |]
                Nothing -> do
                    [whamlet|
                        <img src="/static/imgnotfound.png"  width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                    |]

fotoByLamina2 :: LaminaId -> Widget
fotoByLamina2 lamid = do 
            zooms <- handlerToWidget $ runDB $ selectFirst [ZoomLamina ==. lamid][]
            case zooms of 
                Just (Entity zid zoom) -> do
                    imgzoom <- handlerToWidget $ runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
                    case imgzoom of
                        Just (Entity imgid img) -> do
                            [whamlet|
                                <img src="/static/zoom/#{imgzoomNmimg img}" width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                            |]
                        Nothing -> do
                            [whamlet|
                                <img src="/static/imgnotfound.png" width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                            |]
                Nothing -> do
                    [whamlet|
                        <img src="/static/imgnotfound.png" width="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                    |]

fotoByUser2 :: UsuarioId -> Widget
fotoByUser2 lamid = do 
            zooms <- handlerToWidget $ runDB $ selectFirst [ImguserIduser ==. Just lamid][]
            case zooms of 
                Just (Entity zid zoom) -> do
                            [whamlet|
                                <img src="/static/usuarios/#{imguserNmimg zoom}" height="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                            |]
                Nothing -> do
                    [whamlet|
                        <img src="/static/images/default/anonimo.jpg" height="100%" style="max-width:200px; height:150px; border-sizing:border-box"></img>
                    |]

myProfile :: Text -> Widget
myProfile txt = do
    usuario <- handlerToWidget $ runDB $ selectFirst [UsuarioLogin ==. txt][]
    case usuario of
        Just (Entity usuid logged) -> do 
            [whamlet| <a href=@{PerfilR usuid}>Perfil</a></li>|]
        Nothing -> do
            [whamlet| <script>alert("Algo deu errado")</script>|]