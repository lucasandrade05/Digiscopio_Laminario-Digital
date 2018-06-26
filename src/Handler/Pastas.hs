{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Pastas where

import Import
import Database.Persist.Postgresql
import Handler.Pesq
import Handler.Formes

formCadPastas :: Form Pasta
formCadPastas = renderBootstrap $ Pasta
    <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Digite o nome da pasta a ser criada:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Minha Pasta Favorita")]} Nothing
    <*> aopt hiddenField "" Nothing
    <*> aopt hiddenField "" Nothing

getPastasR :: Handler Html
getPastasR = do
    userlogado <- lookupSession "_ID"
    case userlogado of
        Just logado -> do
            user <- runDB $ selectFirst [UsuarioLogin ==. logado][]
            case user of
                Just (Entity usuario proprietario) -> do 
                    let pid = usuarioRaiz proprietario :: PastaId
                    pastlist <- runDB $ selectList [PastaRoot ==. Just (usuarioRaiz proprietario)][]
                    lamlist <- runDB $ selectList [LaminaUsuario ==. usuario][]
                    content <- runDB $ selectList [ConteudoIdpasta ==. pid][]
                    imagem <- runDB $ selectFirst [ImguserIduser ==. (Just usuario)][]
                    (widget, enctype) <- generateFormPost formCadPastas
                    (widget3, enctype3) <- generateFormPost formCadLamina
                    defaultLayout $ do 
                        setTitle "Digiscópio - Minha Pasta - Laminário Digital"
                        addStylesheet $ (StaticR css_bootstrap_css)
                        addScript $ StaticR js_jquery_min_js
                        addScript $ StaticR js_bootstrap_js
                        toWidgetHead[lucius| body{background-color:#000033;}|]
                        toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                        toWidget $ $(whamletFile "templates/menu.hamlet")  
                        toWidget $ $(whamletFile "templates/pastas.hamlet")
                Nothing -> do
                    redirect HomeR
        Nothing -> do
            redirect HomeR
            
postPastasR :: Handler Html
postPastasR = do
    userlogado <- lookupSession "_ID"
    case userlogado of
      Just usersess -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. usersess][]
        case user of
            Just (Entity usuid logged) -> do 
                raiz <- runDB $ selectFirst[PastaId ==. usuarioRaiz logged][]
                case raiz of
                    Just (Entity opcid root) -> do 
                            ((result, _), _) <- runFormPost formCadPastas
                            case result of
                                FormSuccess pastasok -> do -- formulario ok
                                    pastaId <- runDB $ insert $ pastasok -- insere no banco
                                    _ <- runDB $ update pastaId [PastaRoot =. Just opcid, PastaUsuario =. Just usuid, PastaNome =. "asodaisdhosuiahdy"] -- insere a raiz e o proprietario
                                    _ <- runDB $ insert $ Conteudo {conteudoIdpasta = opcid, conteudoIdpast = (Just pastaId), conteudoIdlam = Nothing}
                                    let nome = pastaNome pastasok
                                    updatepasta pastaId nome nome 1
                                    redirect PastasR
                                _ -> do
                                    setMessage $ [shamlet| Dados invalidos! |] 
                                    redirect LoginR
                    Nothing -> do
                        setMessage [shamlet| <script> alert("erro na raiz");</script> |] 
                        redirect PastasR                        
            Nothing -> do
                setMessage [shamlet| <script> alert("erro no usuario");</script> |] 
                redirect PastasR
      Nothing -> do
            setMessage [shamlet| <script> alert("erro no id do usuario");</script> |] 
            redirect PastasR

postConteudoR :: PastaId -> Handler Html
postConteudoR pasta = do
    usersess <- lookupSession "_ID"
    case usersess of
      Just userlog -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. userlog][]
        case user of
            Just (Entity usuid logged) -> do 
                ((result, _), _) <- runFormPost formCadPastas
                case result of
                    FormSuccess pastasok -> do -- formulario ok
                        pastaId <- runDB $ insert $ pastasok -- insere no banco
                        _ <- runDB $ update pastaId [PastaRoot =. Just pasta, PastaUsuario =. Just usuid, PastaNome =. "asodaisdhosuiahdy"] -- insere a raiz e o proprietario
                        _ <- runDB $ insert $ Conteudo {conteudoIdpasta = pasta, conteudoIdpast = (Just pastaId), conteudoIdlam = Nothing}
                        let nome = pastaNome pastasok
                        updatepasta pastaId nome nome 1
                        redirect (ConteudoR pasta)
                    _ -> do
                        setMessage $ [shamlet| Dados invalidos! |] 
                        redirect LoginR
            Nothing -> do
                setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
                redirect PastasR            
      Nothing -> do
            setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
            redirect PastasR

            
caminhoRaiz :: PastaId -> Widget
caminhoRaiz pid = do
        atual <- handlerToWidget $ runDB $ get404 pid --PASTA ATUAL
        caminho <- case (pastaRoot atual) of 
            Just x -> handlerToWidget $ runDB $ selectFirst [PastaId ==. x][] -- PASTA RAIZ DA ATUAL
            Nothing -> redirect PastasR
        case caminho of
            Just (Entity opcid raiz) -> do
                if fromSqlKey(opcid) >= 2  -- VERIFICA SE NAO É A RAIZ DO PROJETO
                    then do
                        [whamlet|
                        <a href=@{ConteudoR pid} style="color:white">#{pastaNome atual}</a> / 
                        |]
                        caminhoRaiz(opcid) -- CHAMA A RECURSÃO
                    else if fromSqlKey(opcid) == 1
                        then do
                            [whamlet|
                            <a href=@{ConteudoR pid} style="color:white">#{pastaNome atual}</a> / 
                            |]
                        else do
                            [whamlet|  |]
            Nothing -> do
                [whamlet|
                    
                |]
            



getConteudoR :: PastaId -> Handler Html
getConteudoR pid = do
    userlogado <- lookupSession "_ID"
    atual <- runDB $ get404 pid
    let user = pastaUsuario atual
    case user of 
        Just usuario -> do
            proprietario <- runDB $ get404 usuario
            (widget, enctype) <- generateFormPost formCadPastas
            (widget3, enctype3) <- generateFormPost formCadLamina
            content <- runDB $ selectList [ConteudoIdpasta ==. pid][Asc ConteudoIdpast] 
            imagem <- runDB $ selectFirst [ImguserIduser ==. (Just usuario)][]
            lamlist <- runDB $ selectList [LaminaUsuario ==. usuario][]
            defaultLayout $ do 
                setTitle "Digiscópio - Pastas - Laminário Digital"
                addStylesheet $ (StaticR css_bootstrap_css)
                addScript $ StaticR js_jquery_min_js
                addScript $ StaticR js_bootstrap_js
                toWidgetHead[lucius| body{background-color:#000033;}|]
                toWidgetHead[hamlet|<meta name="viewport" content="width=device-width, initial-scale=1">|]
                toWidget $ $(whamletFile "templates/menu.hamlet")  
                toWidget $ $(whamletFile "templates/cpastas.hamlet")


getAddConteudoR :: PastaId -> LaminaId -> Handler Html
getAddConteudoR pid lamid = do
    _ <- runDB $ insert $ Conteudo {conteudoIdpasta = pid, conteudoIdpast = Nothing, conteudoIdlam = (Just lamid)}
    setMessage $ [shamlet| <script>alert("Lamina inserida no diretório com sucesso!")</script>|]
    redirect (ConteudoR pid)
    
userpicById :: UsuarioId -> Widget
userpicById usuid = do 
    imagem <- handlerToWidget $ runDB $ selectFirst [ImguserIduser ==. (Just usuid)][]
    [whamlet|
     $forall (Entity img hl) <- imagem
      /static/usuarios/#{imguserNmimg hl}
      |]
    
postDelPastaR :: PastaId -> Handler Html 
postDelPastaR pid = do 
    pasta <- runDB $ get404 pid
    deletaPasta(pid)
    let root = (pastaRoot pasta) :: Maybe PastaId
    case root of
        Just rid -> redirect (ConteudoR rid)
        Nothing -> redirect HomeR

postEditPastaR :: PastaId -> Handler Html 
postEditPastaR pid = do 
    pasta <- runDB $ get404 pid
    pasttit <- runInputPost $ ireq textField "pastanova"
    updatepasta pid pasttit pasttit 1
    let root = (pastaRoot pasta) :: Maybe PastaId
    case root of
        Just rid -> redirect (ConteudoR rid)
        Nothing -> redirect HomeR
        
updatepasta :: PastaId -> Text -> Text -> Int -> Handler ()
updatepasta pid nome verifica num = do
    pasta <- runDB $ get404 pid
    let raiz = (pastaRoot pasta) :: Maybe PastaId
    case raiz of 
        Just rid -> do
            conteudo <- runDB $ selectFirst [PastaRoot ==. Just rid, PastaNome ==. verifica][]
            case conteudo of
                Just cid -> do
                    let n = num+1
                    let new = ((unpack(nome)) ++ " (" ++ show(n) ++ ")")
                    let verifica = pack(new)
                    updatepasta pid nome verifica n  
                Nothing -> do
                    runDB $ update pid [PastaNome =. verifica]        
                    setMessage [shamlet| <script> alert("Pasta inserida/alterada com sucesso!")</script>|]
        Nothing -> do
            setMessage [shamlet| <script> alert("ERRO")</script>|]
            redirect HomeR

        
postDelPastR :: ConteudoId -> Handler Html
postDelPastR pid = do
    conteudo <- runDB $ get404 pid
    runDB (delete pid)
    redirect (ConteudoR (conteudoIdpasta conteudo))

deletaPasta :: PastaId -> Handler ()
deletaPasta pid = do
    pasta <- runDB $ get404 pid
    pastadentro <- runDB $ selectFirst [PastaRoot ==. Just pid][]
    case pastadentro of
        Just (Entity pdid _) -> do
            deletaPastaC (pdid)
            deletaPasta (pid)
        Nothing -> do
            cpasta <- runDB $ selectFirst [ConteudoIdpasta ==. pid][Desc ConteudoId]
            case cpasta of
                Just (Entity cid cont) -> do
                    runDB $ delete(cid)
                    deletaPasta (pid)
                Nothing -> do
                    cdentro <- runDB $ selectFirst [ConteudoIdpast ==. Just pid][]
                    case cdentro of 
                        Just (Entity cid cont) -> do
                            runDB $ delete(cid)
                            deletaPasta (pid)
                        Nothing -> do
                            runDB $ delete(pid)
                            
deletaPastaC :: PastaId -> Handler ()
deletaPastaC pid = do
    pasta <- runDB $ get404 pid
    pastadentro <- runDB $ selectFirst [PastaRoot ==. Just pid][]
    case pastadentro of
        Just (Entity pdid _) -> do
            deletaPasta(pdid)
            deletaPastaC(pid)
        Nothing -> do
            cpasta <- runDB $ selectFirst [ConteudoIdpasta ==. pid][Desc ConteudoId]
            case cpasta of
                Just (Entity cid cont) -> do
                    runDB $ delete(cid)
                    deletaPastaC(pid)
                Nothing -> do
                    cdentro <- runDB $ selectFirst [ConteudoIdpast ==. Just pid][]
                    case cdentro of 
                        Just (Entity cid cont) -> do
                            runDB $ delete(cid)
                            deletaPastaC(pid)
                        Nothing -> do
                            runDB $ delete(pid)


      
tituloLam :: LaminaId -> Widget
tituloLam lamid = do
    lamina <- handlerToWidget $ runDB $ get404 lamid
    [whamlet|
            #{laminaTitulo lamina}
    |]
    
tituloPasta :: PastaId -> Widget
tituloPasta pastid = do
    pasta <- handlerToWidget $ runDB $ get404 pastid
    [whamlet|
            #{pastaNome pasta}
    |]
    
addlaminaconteudo :: LaminaId -> PastaId -> Widget
addlaminaconteudo lid pid = do
    verifica <- handlerToWidget $ runDB $ selectFirst [ConteudoIdpasta ==. pid, ConteudoIdlam ==. (Just lid)][]
    lamina <- handlerToWidget $ runDB $ get404 lid
    case verifica of
        Just (Entity _ _)-> do 
            [whamlet||]
        Nothing -> do
            [whamlet|
             <div class="col-md-4 col-lg-4 col-sm-4 col-xs-6" style="padding: 1%">
              <table>
               <tr><td><a href=@{AddConteudoR pid lid}><center><button><img src="/static/images/default/lamina.jpg" height="100px" width="100px"></img></button></td></tr>
               <tr><td><a href=@{AddConteudoR pid lid}><center>#{laminaTitulo lamina}</td></tr>
            |]