{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql
import Handler.Pesq
import Handler.UserHome


getAdmControlR :: Int -> Handler Html
getAdmControlR page = do
    contagem <- runDB $ count[UsuarioId >. toSqlKey(0)]
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
    userlist <- runDB $ selectList [][Asc UsuarioId, LimitTo 8, OffsetBy $ (page - 1) * 8]
    userlogado <- lookupSession "_ID"
    defaultLayout $ do 
        setTitle "Digiscópio - ADM CONTROL - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet")  
        toWidget $ $(whamletFile "templates/admcontrol.hamlet")


postAdmDelUserR :: UsuarioId -> Int -> Handler Html
postAdmDelUserR userid page = do
    deletaUser(userid)
    deleteSession "_ID"
    setMessage $ [shamlet|<script> alert("Conta deletada com sucesso.")</script>|]
    redirect (AdmControlR page)
    
postAdmEditUserR :: UsuarioId -> Int -> Handler Html
postAdmEditUserR usuid page = do
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
        redirect (AdmControlR page)
    else do
        case mail of
            Just (Entity _ _) -> do
                setMessage $ [shamlet|<script>alert("Este e-mail já está em uso!");</script>|]
                redirect (AdmControlR page)
            Nothing -> do
                case senha of 
                    Just senhaa -> do
                        case senha2 of
                            Just senhaa2 -> do
                                if senha /= senha2 then do
                                    setMessage $ [shamlet|<script> alert("Erro na confirmação da senha. Digite a nova senha novamente nos dois campos.");</script>|]
                                    redirect (AdmControlR page)
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
                                                                    redirect (AdmControlR page)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                        Nothing -> do
                                                            redirect (AdmControlR page)
                                                Nothing -> do
                                                    runDB $ update usuid [UsuarioOcultar =. False]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                        Nothing -> do
                                                            redirect (AdmControlR page)
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
                                                                    redirect (AdmControlR page)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                        Nothing -> do
                                                            redirect (AdmControlR page)
                                                Nothing -> do
                                                    runDB $ update usuid [UsuarioOcultar =. False]
                                                    case foto of
                                                        Just pic -> do
                                                            fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                            case fotouser of
                                                                Just (Entity picture _) -> do
                                                                    deluserimg(picture)
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                                Nothing -> do
                                                                    writeToServeruser pic usuid
                                                                    redirect (AdmControlR page)
                                                        Nothing -> do
                                                            redirect (AdmControlR page)
                            Nothing -> do
                                setMessage $ [shamlet|<script> alert("Erro na confirmação da senha. Digite a nova senha novamente no segundo campo.");</script>|]
                                redirect (AdmControlR page)
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
                                                        redirect (AdmControlR page)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                            Nothing -> do
                                                redirect (AdmControlR page)
                                    Nothing -> do
                                        runDB $ update usuid [UsuarioOcultar =. False]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                            Nothing -> do
                                                redirect (AdmControlR page)
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
                                                        redirect (AdmControlR page)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                            Nothing -> do
                                                redirect (AdmControlR page)
                                    Nothing -> do
                                        runDB $ update usuid [UsuarioOcultar =. False]
                                        case foto of
                                            Just pic -> do
                                                fotouser <- runDB $ selectFirst [ImguserIduser ==. Just usuid][]
                                                case fotouser of
                                                    Just (Entity picture _) -> do
                                                        deluserimg(picture)
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                                    Nothing -> do
                                                        writeToServeruser pic usuid
                                                        redirect (AdmControlR page)
                                            Nothing -> do
                                                redirect (AdmControlR page)
