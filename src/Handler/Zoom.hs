{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Zoom where

import System.Directory
import Import
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Text.Julius
import Handler.Formes
import Handler.Pesq

postCadZoomR :: LaminaId -> Handler Html
postCadZoomR cid = do
    usersess <- lookupSession "_ID"
    case usersess of
      Just userlog -> do
        user <- runDB $ selectFirst [UsuarioLogin ==. userlog][]
        case user of
            Just (Entity usuid logged) -> do 
                ((result, _), _) <- runFormPost formCadZoom
                case result of
                    FormSuccess zoomok -> do
                        let zoom = formeZoom2 zoomok cid usuid
                        verifica <- runDB $ selectFirst [ZoomLamina ==. cid, ZoomCorte ==. (zoomCorte zoom), ZoomAmpliacao ==. (zoomAmpliacao zoom)][]
                        case verifica of
                            Just (Entity tem _) -> do
                                setMessage $ [shamlet| <script> alert("Já existe uma imagem com esta ampliação para o corte selecionado referente esta lamina. O cadastro não pode ser concluído. Favor digitar outra ampliação ou inserir um destaque.");</script>|]
                                redirect (VerLaminaCR cid (zoomCorte zoom))
                            Nothing -> do
                                zoomId <- runDB $ insert $ formeZoom2 zoomok cid usuid
                                filename <- writeToServer (formeFile2(zoomok)) zoomId
                                redirect (VerLaminaVR cid (zoomCorte zoom) zoomId)
                    _ -> do
                        setMessage $ [shamlet| Dados invalidos! |] 
                        redirect LoginR
            Nothing -> do
                    setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
                    redirect HomeR
      Nothing -> do
            setMessage [shamlet| <script> alert("Algo deu errado... Você precisa estar logado para cadastrar!");</script> |] 
            redirect HomeR
            
        
zoomById :: ZoomId -> Widget
zoomById zid = do 
    imagem <- handlerToWidget $ runDB $ selectFirst [ImgzoomIdzoom ==. (Just zid)][]
    [whamlet|
     $forall (Entity img zoom) <- imagem
      /static/cortes/#{imgzoomNmimg zoom}
      |]
        
        
imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static/zoom/"

writeToServer :: FileInfo -> ZoomId -> Handler FilePath
writeToServer file zid = do
    let filename = "ZOOM_" ++ (show $ (fromSqlKey zid)) ++ "_" ++ (unpack $ fileName file)
        path = imageFilePath filename
    liftIO $ fileMove file path
    _ <- runDB $ insert (Imgzoom filename (Just zid))
    return filename