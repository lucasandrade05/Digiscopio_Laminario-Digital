{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.UserHome where

import Import
import Database.Persist.Postgresql
import Handler.Pesq
import System.Directory (removeFile, doesFileExist)
import Handler.Lamina (deletaLamina)


getPerfilR :: UsuarioId -> Handler Html
getPerfilR userid = do
    (widget2, enctype2) <- generateFormPost formPesquisa
    userlogado <- lookupSession "_ID"
    user <- runDB $ get404 userid
    imagem <- runDB $ selectFirst [ImguserIduser ==. (Just userid)][]
    defaultLayout $ do 
                setTitle "Digiscópio - Perfis - Laminário Digital"
                addStylesheet $ (StaticR css_bootstrap_css)
                addScript $ StaticR js_jquery_min_js
                addScript $ StaticR js_bootstrap_js
                toWidgetHead[lucius| body{background-color:#000033;}|]
                toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                toWidget $ $(whamletFile "templates/menu.hamlet")  
                toWidget $ $(whamletFile "templates/perfil.hamlet")

postDelUserR :: UsuarioId -> Handler Html
postDelUserR userid = do
    deletaUser(userid)
    deleteSession "_ID"
    setMessage $ [shamlet|<script> alert("Sua conta foi deletada com sucesso.")</script>|]
    redirect HomeR
    
postEditUserR :: UsuarioId -> Handler Html
postEditUserR usuid = do
    nome <- runInputPost $ ireq textField "nome"
    login <- runInputPost $ ireq textField "login"
    email <- runInputPost $ ireq textField "email"
    ocultar <- runInputPost $ iopt textField "ocult"
    biografia <- runInputPost $ iopt textareaField "bio"
    senha <- runInputPost $ iopt passwordField "senha"
    senha2 <- runInputPost $ iopt passwordField "senha2"
    foto <- runInputPost $ iopt fileField "foto"
    senhaatual <- runInputPost $ ireq passwordField "senhaatual"
    user <- runDB $ get404 usuid
    mail <- runDB $ selectFirst [UsuarioEmail ==. email, UsuarioLogin !=. login][]
    if (usuarioSenha user) /= senhaatual then do 
        setMessage $ [shamlet|<script>alert("Digite a senha correta para realizar alterações no cadastro.");</script>|]
        redirect (PerfilR usuid)
    else do
        case mail of
            Just (Entity _ _) -> do
                setMessage $ [shamlet|<script>alert("Este e-mail já está em uso!");</script>|]
                redirect (PerfilR usuid)
            Nothing -> do
                case senha of 
                    Just senhaa -> do
                        case senha2 of
                            Just senhaa2 -> do
                                if senha /= senha2 then do
                                    setMessage $ [shamlet|<script> alert("Erro na confirmação da senha. Digite a nova senha novamente nos dois campos.");</script>|]
                                    redirect (PerfilR usuid)
                                else do
                                    runDB $ update usuid [UsuarioNome =. nome, UsuarioSenha =. senhaa, UsuarioEmail =. email]
                                    case biografia of
                                        Just bio -> do
                                            runDB $ update usuid [UsuarioBiografia =. Just bio]
                                            case ocultar of
                                                Just _ -> do
                                                    runDB $ update usuid [UsuarioOcultar =. True]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                        Nothing -> do
                                                            redirect (PerfilR usuid)
                                                Nothing -> do
                                                    runDB $ update usuid [UsuarioOcultar =. False]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                        Nothing -> do
                                                            redirect (PerfilR usuid)
                                        Nothing -> do
                                            runDB $ update usuid [UsuarioBiografia =. Nothing]
                                            case ocultar of
                                                Just _ -> do 
                                                    runDB $ update usuid [UsuarioOcultar =. True]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                        Nothing -> do
                                                            redirect (PerfilR usuid)
                                                Nothing -> do
                                                    runDB $ update usuid [UsuarioOcultar =. False]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (PerfilR usuid)
                                                        Nothing -> do
                                                            redirect (PerfilR usuid)
                            Nothing -> do
                                setMessage $ [shamlet|<script> alert("Erro na confirmação da senha. Digite a nova senha novamente no segundo campo.");</script>|]
                                redirect (PerfilR usuid)
                    Nothing -> do
                        runDB $ update usuid [UsuarioNome =. nome, UsuarioEmail =. email]
                        case biografia of
                            Just bio -> do
                                runDB $ update usuid [UsuarioBiografia =. Just bio]
                                case ocultar of
                                    Just _ -> do 
                                        runDB $ update usuid [UsuarioOcultar =. True]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                            Nothing -> do
                                                redirect (PerfilR usuid)
                                    Nothing -> do
                                        runDB $ update usuid [UsuarioOcultar =. False]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                            Nothing -> do
                                                redirect (PerfilR usuid)
                            Nothing -> do
                                runDB $ update usuid [UsuarioBiografia =. Nothing]
                                case ocultar of
                                    Just _ -> do
                                        runDB $ update usuid [UsuarioOcultar =. True]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                            Nothing -> do
                                                redirect (PerfilR usuid)
                                    Nothing -> do
                                        runDB $ update usuid [UsuarioOcultar =. False]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (PerfilR usuid)
                                            Nothing -> do
                                                redirect (PerfilR usuid)


deletaUser :: UsuarioId -> Handler ()
deletaUser userid = do
    lamina <- runDB $ selectFirst [LaminaUsuario ==. userid][]
    case lamina of
        Just (Entity lamid _) -> do
            deletaLamina(lamid)
            deletaUser(userid)
        Nothing -> do
            pastas <- runDB $ selectFirst [PastaUsuario ==. Just userid, PastaRoot >. Just (toSqlKey(1) :: Key Pasta)][Desc PastaRoot]
            case pastas of
                Just (Entity pastid pasta) -> do
                    content <- runDB $ selectFirst [ConteudoIdpast ==. Just pastid][]
                    case content of 
                        Just (Entity contid _) -> do
                            runDB $ delete(contid)
                            deletaUser(userid)
                        Nothing -> do
                            contentr <- runDB $ selectFirst [ConteudoIdpasta ==. pastid][]
                            case contentr of
                                Just (Entity contidr _) -> do
                                    runDB $ delete(contidr)
                                    deletaUser(userid)
                                Nothing -> do
                                    let raiz = (pastaRoot pasta)
                                    case raiz of
                                        Just rid -> do 
                                                runDB $ update pastid[PastaUsuario =. Nothing]
                                                runDB $ delete(pastid)
                                                deletaUser(userid)
                Nothing -> do
                    user <- runDB $ get404 userid
                    runDB $ update (usuarioRaiz user) [PastaUsuario =. Nothing]
                    userpic <- runDB $ selectFirst [ImguserIduser ==. Just userid][]
                    case userpic of
                        Just (Entity pic _) -> do
                            deluserimg(pic)
                            runDB $ delete(userid)
                            runDB $ delete (usuarioRaiz user)
                        Nothing -> do
                            runDB $ delete(userid)
                            runDB $ delete (usuarioRaiz user)
                    
                    
deluserimg :: ImguserId -> Handler ()
deluserimg imageId = do
    image <- runDB $ get404 imageId
    let chave = imguserIduser image
    case chave of 
        Just chav -> do
            let path = "static/usuarios/" ++ (imguserNmimg image)
            liftIO $ removeFile path
            -- only delete from database if file has been removed from server
            stillExists <- liftIO $ doesFileExist path
            case (not stillExists) of 
                False  -> redirect HomeR
                True -> do
                    runDB $ delete imageId
                    
imageFilePathuser :: String -> FilePath
imageFilePathuser f = uploadDirectoryuser </> f

uploadDirectoryuser :: FilePath
uploadDirectoryuser = "static/usuarios/"

writeToServeruser :: FileInfo -> UsuarioId -> Handler FilePath
writeToServeruser file zid = do
    let filename = "USER_" ++ (show $ (fromSqlKey zid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePathuser filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imguser filename (Just zid))
    return filename