{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just $ LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized LogoutR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized CadLaminaR _ = ehUsuario
    isAuthorized (CadHighlightR _) _ = ehUsuario
    isAuthorized (AdmControlR  _) _ = ehAdmin
    isAuthorized (AdmDelUserR _ _) _ = ehAdmin
    isAuthorized (AdmEditUserR _ _) _ = ehAdmin
    isAuthorized LoginR _ = usuarioNao
    isAuthorized _ _ = return Authorized

ehAdmin :: Handler AuthResult
ehAdmin = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "admin") -> return Authorized
        (Just _ ) -> return $ Unauthorized "VC NAO EH O PAH!"
    
ehUsuario :: Handler AuthResult
ehUsuario = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just _) -> return Authorized

usuarioNao :: Handler AuthResult
usuarioNao = do
    sessao <- lookupSession "_ID"
    case sessao of
        Nothing -> return Authorized
        (Just _) -> redirect HomeR

--instance Yesod App where
--    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
