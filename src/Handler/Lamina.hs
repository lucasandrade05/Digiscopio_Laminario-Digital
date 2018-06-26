{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Lamina where

import Import
import Database.Persist.Postgresql
import Handler.Formes
import System.Directory (removeFile, doesFileExist)
import Handler.Pesq
import Handler.Highlight (updatehigh)


postCadLaminaR :: Handler Html
postCadLaminaR = do
    usersess <- lookupSession "_ID"
    case usersess of --NOME USER LOGADO
      Just userlog -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. userlog][]
        case user of -- ID USER LOGADO
            Just (Entity usuid _) -> do 
                ((result, _), _) <- runFormPost formCadLamina
                case result of -- FORMULARIO CADASTRO OK
                    FormSuccess laminaok -> do
                        lamina <- runDB $ insert $ formeLam laminaok usuid
                        zoom <- runDB $ insert $ formeZoom laminaok lamina usuid
                        filename <- writeToServerz (formeFile(laminaok)) zoom
                        redirect (VerLaminaR lamina)
                    _ -> do
                        setMessage $ [shamlet|<script> alert("Algo deu errado com os dados preenchidos");</script> |] 
                        redirect CadLaminaR
            Nothing -> do
                    setMessage [shamlet|<script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
                    redirect CadLaminaR
      Nothing -> do
            setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
            redirect CadLaminaR
            
postDelLaminaR :: LaminaId -> Handler Html 
postDelLaminaR pid = do 
    _ <- runDB $ get404 pid
    deletaLamina(pid)
    redirect (ExplorarR 1)

postDelHLR :: HighlightId -> Handler Html
postDelHLR hid = do
    high <- runDB $ get404 hid
    imagem <- runDB $ selectFirst [ImghlIdhl ==. (Just hid)][]
    case imagem of
        Just (Entity imid _) -> do
            delhlimg(imid)
            zoom <- runDB $ get404 (highlightZoom high)
            runDB $ delete hid
            redirect (VerLaminaVR (zoomLamina zoom) (zoomCorte zoom) (highlightZoom high))

postDelZoomR :: ZoomId -> Handler Html
postDelZoomR zid = do
    zoom <- runDB $ get404 zid
    deletaZoom(zid)
    redirect (VerLaminaR (zoomLamina zoom))



postEditarLaminaR :: LaminaId -> Handler Html
postEditarLaminaR lamid = do
    lamtit <- runInputPost $ ireq textField "editlamtit"
    lamdesc <- runInputPost $ ireq textareaField "editlamdes"
    zom <- runInputPost $ ireq hiddenField "zomid"
    corte <- runInputPost $ ireq textField "editcorte"
    amp <- runInputPost $ ireq intField "editamp"
    col <- runInputPost $ ireq textField "editcol"
    zomdes <- runInputPost $ ireq textareaField "editzomdes"
    fotoamp <- runInputPost $ iopt fileField "fotozoom"
    highlight <- runInputPost $ iopt hiddenField "hlid"
    hltit <- runInputPost $ iopt textField "edithltit"
    hldes <- runInputPost $ iopt textareaField "edithldes"
    fotohl <- runInputPost $ iopt fileField "fotohid"
    runDB $ update lamid [LaminaTitulo =. lamtit, LaminaDescricao =. lamdesc]
    let zid = toSqlKey(zom) :: Key Zoom
    runDB $ update zid [ZoomCorte =. corte, ZoomAmpliacao =. amp, ZoomColoracao =. col, ZoomDescricao =. zomdes]
    case fotoamp of 
        Just picz -> do
            fotoz <- runDB $ selectFirst [ImgzoomIdzoom ==. Just zid][]
            case fotoz of
                Just (Entity piczo _) -> do
                    delzoomimg(piczo)
                    writeToServerz picz zid
                    runDB $ get404 zid
        Nothing -> do
            runDB $ get404 zid
    case highlight of
        Just hli -> do
            let hid = toSqlKey(hli) :: Key Highlight
            case hltit of
                Just titulo -> do
                    updatehigh hid titulo titulo 1
                Nothing -> do
                    setMessage $ [shamlet| <script> alert("Por favor insira um titulo para o destaque!");</script>|]
                    redirect (VerLaminaR lamid)
            case hldes of
                Just descricao -> runDB $ update hid [HighlightDescricao =. descricao]
                Nothing -> do
                    setMessage $ [shamlet| <script> alert("Por favor insira uma descricao para o destaque!");</script>|]
                    redirect (VerLaminaR lamid)
            case fotohl of
                Just foto -> do
                    fotohll <- runDB $ selectFirst [ImghlIdhl ==. Just hid][]
                    case fotohll of
                        Just (Entity pic _) -> do
                            delhlimg(pic)
                            writeToServerhl foto hid
                            redirect (VerLaminaR lamid)
                Nothing -> redirect (VerLaminaR lamid)
        Nothing -> redirect (VerLaminaR lamid)
    redirect (VerLaminaR lamid)
    
getCadLaminaR :: Handler Html
getCadLaminaR = do 
    userlogado <- lookupSession "_ID"
    case userlogado of --NOME USER LOGADO
      Just usersess -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. usersess][]
        case user of -- ID USER LOGADO
            Just (Entity usuid logged) -> do 
                 (widget, enctype) <- generateFormPost formCadLamina
                 defaultLayout $ do
                    setTitle "Digiscópio - Cadastrar Lâmina - Laminário Digital"
                    addStylesheet $ (StaticR css_bootstrap_css)
                    addScript $ StaticR js_jquery_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                    toWidget $ $(whamletFile "templates/menu.hamlet")  
                    toWidget $ $(whamletFile "templates/cadastrarlamina.hamlet")
            Nothing -> do
                redirect LoginR
      Nothing -> do
          redirect LoginR

getVerLaminaR :: LaminaId -> Handler Html
getVerLaminaR laminaid = do
    userlogado <- lookupSession "_ID"
    (formzom, enctype) <- generateFormPost formCadZoom
    (formhl, enctype) <- generateFormPost formCadHL
    (widget2, enctype2) <- generateFormPost formPesquisa
    lamina <- runDB $ get404 laminaid
    usuario <- runDB $ get404 (laminaUsuario lamina)
    zoom <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Transversal"][]
    ctrans <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Transversal"][]
    clong <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Longitudinal"][]
    cobli <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Oblíquo"][]
    case zoom of
        Just (Entity zid zom) -> do
            zooms <- runDB $ selectList [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Transversal"][]
            let corte = "Corte Transversal" :: Text
            highlights <- runDB $ selectList [HighlightZoom ==. zid][]
            imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
            defaultLayout $ do 
                setTitle "Digiscópio - Visualizando Lâmina - Laminário Digital"
                addStylesheet $ (StaticR css_bootstrap_css)
                addScript $ StaticR js_jquery_min_js
                addScript $ StaticR js_bootstrap_js
                toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                toWidget $ $(whamletFile "templates/menu.hamlet")  
                toWidget $ $(whamletFile "templates/lamina.hamlet")
        Nothing -> do
            zoom2 <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Longitudinal"][]
            case zoom2 of
                Just (Entity zid zom) -> do
                    zooms <- runDB $ selectList [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Longitudinal"][]
                    let corte = "Corte Longitudinal" :: Text
                    highlights <- runDB $ selectList [HighlightZoom ==. zid][]
                    imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
                    defaultLayout $ do 
                        setTitle "Digiscópio - Visualizando Lâmina - Laminário Digital"
                        addStylesheet $ (StaticR css_bootstrap_css)
                        addScript $ StaticR js_jquery_min_js
                        addScript $ StaticR js_bootstrap_js
                        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                        toWidget $ $(whamletFile "templates/menu.hamlet")  
                        toWidget $ $(whamletFile "templates/lamina.hamlet")
                Nothing -> do
                    zoom3 <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Oblíquo"][]
                    case zoom3 of
                        Just (Entity zid zom) -> do
                            zooms <- runDB $ selectList [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Oblíquo"][]
                            let corte = "Corte Oblíquo" :: Text
                            highlights <- runDB $ selectList [HighlightZoom ==. zid][]
                            imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
                            defaultLayout $ do 
                                let nomeLamina = "#" ++ (laminaTitulo lamina)  :: Text
                                setTitle "Digiscópio - Visualizando Lâmina - Laminário Digital"
                                addStylesheet $ (StaticR css_bootstrap_css)
                                addScript $ StaticR js_jquery_min_js
                                addScript $ StaticR js_bootstrap_js
                                toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                                toWidget $ $(whamletFile "templates/menu.hamlet")  
                                toWidget $ $(whamletFile "templates/lamina.hamlet")
                        Nothing -> do
                            setMessage $ [shamlet| <script>alert("A lamina não foi cadastrada com sucesso e está corrompida.")</script>|]
                            deletaLamina(laminaid)
                            redirect HomeR

getVerLaminaCR :: LaminaId -> Text -> Handler Html
getVerLaminaCR laminaid corte = do
    userlogado <- lookupSession "_ID"
    (formzom, enctype) <- generateFormPost formCadZoom
    (formhl, enctype) <- generateFormPost formCadHL
    lamina <- runDB $ get404 laminaid
    usuario <- runDB $ get404 (laminaUsuario lamina)
    zoom <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. corte][]
    ctrans <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Transversal"][]
    clong <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Longitudinal"][]
    cobli <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Oblíquo"][]
    case zoom of
        Just (Entity zid zom) -> do
            zooms <- runDB $ selectList [ZoomLamina ==. laminaid, ZoomCorte ==. corte][]
            highlights <- runDB $ selectList [HighlightZoom ==. zid][]
            imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
            defaultLayout $ do 
                setTitle "Digiscópio - Visualizando Lâmina - Laminário Digital"
                addStylesheet $ (StaticR css_bootstrap_css)
                addScript $ StaticR js_jquery_min_js
                addScript $ StaticR js_bootstrap_js
                toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                toWidget $ $(whamletFile "templates/menu.hamlet")  
                toWidget $ $(whamletFile "templates/lamina.hamlet")
        Nothing -> do
            setMessage $ [shamlet| <script>alert("A lamina não foi cadastrada com sucesso e está corrompida.")</script>|]
            redirect HomeR

getVerLaminaVR :: LaminaId -> Text -> ZoomId -> Handler Html
getVerLaminaVR laminaid corte zid = do
    userlogado <- lookupSession "_ID"
    (formzom, enctype) <- generateFormPost formCadZoom
    (formhl, enctype) <- generateFormPost formCadHL
    (widget2, enctype2) <- generateFormPost formPesquisa
    lamina <- runDB $ get404 laminaid
    usuario <- runDB $ get404 (laminaUsuario lamina)
    zom <- runDB $ get404 zid
    ctrans <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Transversal"][]
    clong <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Longitudinal"][]
    cobli <- runDB $ selectFirst [ZoomLamina ==. laminaid, ZoomCorte ==. "Corte Oblíquo"][]
    zooms <- runDB $ selectList [ZoomLamina ==. laminaid, ZoomCorte ==. corte][]
    highlights <- runDB $ selectList [HighlightZoom ==. zid][]
    imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
    defaultLayout $ do 
        setTitle "Digiscópio - Visualizando Lâmina - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/lamina.hamlet")

zoomById :: ZoomId -> Widget
zoomById cid = do 
    imagem <- handlerToWidget $ runDB $ selectFirst [ImgzoomIdzoom ==. (Just cid)][]
    [whamlet|
     $forall (Entity img zoom) <- imagem
      /static/zoom/#{imgzoomNmimg zoom}
      |]
        
hlById :: HighlightId -> Widget
hlById idHl = do 
    imagem <- handlerToWidget $ runDB $ selectFirst [ImghlIdhl ==. (Just idHl)][]
    [whamlet|
     $forall (Entity img hl) <- imagem
      /static/highlights/#{imghlNmimg hl}
      |]
      

delhlimg :: ImghlId -> Handler ()
delhlimg imageId = do
    image <- runDB $ get404 imageId
    let chave = imghlIdhl image
    case chave of 
        Just chav -> do
            let path = "static/highlights/" ++ (imghlNmimg image)
            liftIO $ removeFile path
            -- only delete from database if file has been removed from server
            stillExists <- liftIO $ doesFileExist path
            case (not stillExists) of 
                False  -> redirect HomeR
                True -> do
                    runDB $ delete imageId

delzoomimg :: ImgzoomId -> Handler ()
delzoomimg imageId = do
    image <- runDB $ get404 imageId
    let chave = imgzoomIdzoom image
    case chave of
        Just chav -> do
            let path = "static/zoom/" ++ (imgzoomNmimg image)
            liftIO $ removeFile path
            -- only delete from database if file has been removed from server
            stillExists <- liftIO $ doesFileExist path
            case (not stillExists) of 
                False  -> redirect HomeR
                True -> do
                    runDB $ delete imageId
                    
deletaLamina :: LaminaId -> Handler ()
deletaLamina lamid = do
    zooms <- runDB $ selectFirst [ZoomLamina ==. lamid][]
    case zooms of
        Just (Entity zid _) -> do
            destaques <- runDB $ selectFirst [HighlightZoom ==. zid][]
            case destaques of
                Just (Entity hid _) -> do   --recursivo ate deletar todos os destaques
                    _ <- runDB $ get404 hid
                    imagem <- runDB $ selectFirst [ImghlIdhl ==. (Just hid)][]
                    case imagem of
                        Just (Entity imid _) -> do
                            delhlimg(imid)
                            runDB (delete hid)
                            deletaLamina lamid
                Nothing -> do  -- nao tem mais destaque, roda ate deletar todos os zooms
                    imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
                    case imagem of
                        Just (Entity imid _) -> do
                            delzoomimg(imid)
                            runDB (delete zid)
                            deletaLamina lamid
        Nothing -> do -- nao tem mais zoom
            conteudo <- runDB $ selectFirst [ConteudoIdlam ==. Just lamid][]
            case conteudo of  -- verifica se a lamina esta inserida em alguma pasta
                Just (Entity cid _) -> do
                    runDB (delete cid)
                    deletaLamina lamid
                Nothing -> do
                    runDB (delete lamid)
                    setMessage [shamlet| <script> alert("Lamina deletada com sucesso!")</script>|]
                
deletaZoom :: ZoomId -> Handler ()
deletaZoom zid = do
    destaques <- runDB $ selectFirst [HighlightZoom ==. zid][]
    case destaques of
        Just (Entity hid _) -> do   --recursivo ate deletar todos os destaques
            _ <- runDB $ get404 hid
            imagem <- runDB $ selectFirst [ImghlIdhl ==. (Just hid)][]
            case imagem of
                Just (Entity imid _) -> do
                    delhlimg(imid)
                    runDB (delete hid)
                    deletaZoom zid
        Nothing -> do  -- nao tem mais destaque, roda ate deletar todos os zooms
            imagem <- runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
            case imagem of
                Just (Entity imid _) -> do
                    delzoomimg(imid)
                    runDB (delete zid)
                    setMessage $ [shamlet| <script> alert("Imagem/Ampliação deletada com sucesso!");</script>|]

imageFilePathhl :: String -> FilePath
imageFilePathhl f = uploadDirectoryhl </> f

uploadDirectoryhl :: FilePath
uploadDirectoryhl = "static/highlights/"

writeToServerhl :: FileInfo -> HighlightId -> Handler FilePath
writeToServerhl file highid = do
    let filename = "HL_" ++ (show $ (fromSqlKey highid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePathhl filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imghl filename (Just highid))
    return filename
    
imageFilePathz :: String -> FilePath
imageFilePathz f = uploadDirectoryz </> f

uploadDirectoryz :: FilePath
uploadDirectoryz = "static/zoom/"

writeToServerz :: FileInfo -> ZoomId -> Handler FilePath
writeToServerz file zid = do
    let filename = "ZOOM_" ++ (show $ (fromSqlKey zid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePathz filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imgzoom filename (Just zid))
    return filename