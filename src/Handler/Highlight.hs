{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Highlight where

import System.Directory
import Import
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Text.Julius
import Handler.Pesq
import Handler.Formes

postCadHighlightR :: ZoomId -> Handler Html
postCadHighlightR zoomid = do
    usersess <- lookupSession "_ID"
    zoom <- runDB $ get404 zoomid
    case usersess of
      Just userlog -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. userlog][]
        case user of
            Just (Entity usuid logged) -> do 
                ((result,_),_) <- runFormPost $ formCadHL
                case result of
                    FormSuccess highlight -> do
                        highid <- runDB $ insert $ formeHighlight highlight zoomid usuid
                        high <- runDB $ get404 highid
                        let nome = highlightTitulo high
                        _ <- runDB $ update highid [HighlightTitulo =. "temporar"]
                        updatehigh highid nome nome 1
                        filename <- writeToServer (formeFileHL(highlight)) highid
                        redirect (VerLaminaVR (zoomLamina zoom) (zoomCorte zoom) zoomid)
                    _ -> do
                        setMessage $ [shamlet| Dados invalidos! |] 
                        redirect (VerLaminaVR (zoomLamina zoom) (zoomCorte zoom) zoomid)
            Nothing -> do
                setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
                redirect (VerLaminaVR (zoomLamina zoom) (zoomCorte zoom) zoomid)
      Nothing -> do
            redirect (VerLaminaVR (zoomLamina zoom) (zoomCorte zoom) zoomid)
 
updatehigh :: HighlightId -> Text -> Text -> Int -> Handler ()
updatehigh pid nome verifica num = do
    pasta <- runDB $ get404 pid
    let raiz = (highlightZoom pasta) :: ZoomId
    conteudo <- runDB $ selectFirst [HighlightZoom ==. raiz, HighlightTitulo ==. verifica][]
    case conteudo of
        Just cid -> do
            let n = num+1
            let new = ((unpack(nome)) ++ " (" ++ show(n) ++ ")")
            let verifica = pack(new)
            updatehigh pid nome verifica n  
        Nothing -> do
            runDB $ update pid [HighlightTitulo =. verifica]        
            setMessage [shamlet| <script> alert("Destaque inserido/alterado com sucesso!")</script>|]
        

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static/highlights/"

writeToServer :: FileInfo -> HighlightId -> Handler FilePath
writeToServer file highid = do
    let filename = "HL_" ++ (show $ (fromSqlKey highid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePath filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imghl filename (Just highid))
    return filename