{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql
import Handler.Pesq

import qualified Network.Mail.Mime as Mail


data FormeUser = FormeUser {  x :: Text
                             ,y :: Text
                             ,z :: Text
                             ,w :: Text
                             ,ww :: Text
                             ,yz :: Bool
                             ,o :: PastaId
                             ,p :: Maybe FileInfo
                          }

formCadUser :: PastaId -> Bool -> Form FormeUser
formCadUser root hide = renderDivs $ FormeUser
    <$> areq textField FieldSettings{fsId=Just "nm",
                           fsLabel="Nome Completo",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq emailField FieldSettings{fsId=Just "email",
                           fsLabel="E-mail",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "log",
                           fsLabel="Login",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq passwordField FieldSettings{fsId=Just "psw",
                           fsLabel="Senha",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq passwordField FieldSettings{fsId=Just "psw",
                           fsLabel="Confirme sua senha",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> pure hide
    <*> pure root
    <*> aopt fileField FieldSettings{fsId=Just "fi",
                           fsLabel="Foto (Opcional) : ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control-file"),("placeholder","Selecione:"),("style","border-sizing:border-box")]} Nothing 

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
    <$> areq textField FieldSettings{fsId=Just "li",
                           fsLabel="Login",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq passwordField FieldSettings{fsId=Just "qt",
                           fsLabel="Senha",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar login senha = runDB $ selectFirst [UsuarioLogin ==. login
                                             ,UsuarioSenha ==. senha] []
    
getLoginR :: Handler Html
getLoginR = do 
    userlogado <- lookupSession "_ID"
    (widget,enctype) <- generateFormPost formLogin
    defaultLayout $ do 
        setTitle "Digiscópio - Logar - Laminário Digital"
        addStylesheet $ (StaticR css_bootstrap_css)
        addScript $ StaticR js_jquery_min_js
        addScript $ StaticR js_bootstrap_js
        toWidgetHead[lucius| body{background-color:#000033;}|]
        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
        toWidget $ $(whamletFile "templates/menu.hamlet") 
        toWidget $ $(whamletFile "templates/login.hamlet") 

postLoginR :: Handler Html
postLoginR = do
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("root@root.com","root") -> do 
            setSession "_ID" "admin"
            setMessage $ [shamlet| <script>alert("Bem vindo admin!")</script>|]
            redirect HomeR
        FormSuccess (login,senha) -> do 
            usuario <- autenticar login senha 
            case usuario of 
                Nothing -> do 
                    setMessage $ [shamlet| <script> alert("Usuario ou senha inválidos");</script>|]
                    redirect LoginR 
                Just (Entity usuid usuario) -> do 
                    setSession "_ID" (usuarioLogin usuario)
                    redirect HomeR
        _ -> redirect HomeR
                

postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ID"
    redirect HomeR

formeUs :: FormeUser -> Usuario
formeUs (FormeUser a b c d g zx e f) = (Usuario a b c d Nothing zx e)

nomeUs :: FormeUser -> Text
nomeUs (FormeUser a b c d g zx e f) = c

emailUs :: FormeUser -> Text
emailUs (FormeUser a b c d g zx e f) = b

senhaUs :: FormeUser -> Text
senhaUs (FormeUser a b c d g zx e f) = d

senhaUs2 :: FormeUser -> Text
senhaUs2 (FormeUser a b c d g zx e f) = g

formeFileUser :: FormeUser -> Maybe FileInfo
formeFileUser (FormeUser a b c d g zx e f) =  f


postCadUserR :: Handler Html
postCadUserR =  do 
    let raiz = toSqlKey(1)
    ((result,_),_) <- runFormPost (formCadUser raiz False)
    case result of
        FormSuccess usuario -> do
            users <- runDB $ selectFirst [UsuarioLogin ==. nomeUs(usuario)][]
            case users of
                Just (Entity _ _ ) -> do
                    setMessage $ [shamlet| <script> alert("Este login já está em uso.");</script>|]
                    redirect HomeR
                Nothing -> do
                    email <- runDB $ selectFirst [UsuarioEmail ==. emailUs(usuario)][]
                    case email of
                        Just (Entity _ _) -> do
                            setMessage $ [shamlet| <script> alert("Este e-mail já está em uso.");</script>|]
                            redirect HomeR
                        Nothing -> do
                            if senhaUs(usuario) == senhaUs2(usuario) then do
                                user <- runDB $ insert $ formeUs usuario
                                root <- runDB $ insert Pasta {pastaNome = (nomeUs usuario), pastaRoot = (Just raiz), pastaUsuario = (Just user)}
                                runDB $ update user [UsuarioRaiz =. root]
                                case formeFileUser(usuario) of
                                    Just arq -> do 
                                        filename <- writeToServer arq user
                                        setMessage $ [shamlet| <script> alert("Cadastro efetuado com sucesso!")</script>|]
                                        redirect HomeR
                                    Nothing -> do
                                        setMessage $ [shamlet| <script> alert("Cadastro efetuado com sucesso!")</script>|]
                                        redirect HomeR          
                            else do
                                setMessage $ [shamlet| <script> alert("Por favor digite a mesma senha nos dois campos. Os dados que você enviou não conferem.");</script>|]
                                redirect HomeR
        _ -> do
            setMessage $ [shamlet| <script> alert("Dados invalidos!");</script> |] 
            redirect CadUserR
            
            
imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static/usuarios/"

writeToServer :: FileInfo -> UsuarioId -> Handler FilePath
writeToServer file highid = do
    let filename = "USER_" ++ (show $ (fromSqlKey highid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePath filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imguser filename (Just highid))
    return filename

