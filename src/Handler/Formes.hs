{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Formes where

import System.Directory
import Import
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Text.Julius

data Forme = Forme {    j :: Text
                       ,k :: Textarea
                       ,a :: Text
                       ,b :: Int
                       ,l :: Text
                       ,c :: Textarea
                       ,d :: FileInfo
                    }
                    
data FormeHL = FormeHL { e :: Text
                        ,f :: Textarea
                        ,g :: FileInfo
                    }

data FormeZoom = FormeZoom{  p :: Text
                            ,q :: Int
                            ,r :: Text
                            ,s :: Textarea
                            ,t :: FileInfo
                    }
                          
                          


formCadHL :: Form FormeHL
formCadHL = renderBootstrap $ FormeHL
    <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Titulo",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textareaField FieldSettings{fsId=Just "campo2",
                           fsLabel="Descrição",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("style","resize:vertical")]} Nothing
    <*> areq fileField FieldSettings{fsId=Just "fi",
                           fsLabel="Imagem:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control-file")]} Nothing 

formCadLamina ::  Form Forme
formCadLamina = renderBootstrap $ Forme
    <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Titulo",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textareaField FieldSettings{fsId=Just "campo2",
                           fsLabel="Descrição principal:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("style","resize:vertical")]} Nothing
    <*> areq (selectFieldList cortesLista) FieldSettings{fsId=Just "campo1",
                           fsLabel="Corte:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq intField FieldSettings{fsId=Just "campo2",
                           fsLabel="Ampliação:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo3",
                           fsLabel="Coloração/Tingimento:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Descrição da imagem:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("style","resize:vertical")]} Nothing
    <*> areq fileField FieldSettings{fsId=Just "fi",
                           fsLabel="Imagem:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control-file")]} Nothing
 where
    cortesLista :: [(Text, Text)]
    cortesLista = [("Corte Transversal", "Corte Transversal"), ("Corte Longitudinal", "Corte Longitudinal"), ("Corte Oblíquo", "Corte Oblíquo")]

formCadZoom ::  Form FormeZoom
formCadZoom = renderBootstrap $ FormeZoom
    <$> areq (selectFieldList cortesLista) FieldSettings{fsId=Just "campo1",
                           fsLabel="Corte:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq intField FieldSettings{fsId=Just "campo2",
                           fsLabel="Ampliação:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textField FieldSettings{fsId=Just "campo3",
                           fsLabel="Coloração/Tingimento:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing
    <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Descrição:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("style","resize:vertical")]} Nothing
    <*> areq fileField FieldSettings{fsId=Just "fi",
                           fsLabel="Imagem:",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control-file")]} Nothing
 where
    cortesLista :: [(Text, Text)]
    cortesLista = [("Corte Transversal", "Corte Transversal"), ("Corte Longitudinal", "Corte Longitudinal"), ("Corte Oblíquo", "Corte Oblíquo")]

formeLam :: Forme -> UsuarioId -> Lamina
formeLam(Forme j k a b l c d) m = (Lamina j k m)

formeZoom :: Forme -> LaminaId -> UsuarioId -> Zoom
formeZoom (Forme j k a b l c d) m n = (Zoom a b l c m n)

formeHighlight :: FormeHL -> ZoomId ->  UsuarioId -> Highlight
formeHighlight(FormeHL e f g) h i = (Highlight e f h i)

formeFile :: Forme -> FileInfo
formeFile(Forme j k a b l c d) = d

formeFileHL :: FormeHL -> FileInfo
formeFileHL (FormeHL e f g) = g

formeZoom2Amp :: FormeZoom -> Int
formeZoom2Amp (FormeZoom a b l c d) = b

formeZoom2 :: FormeZoom -> LaminaId -> UsuarioId -> Zoom
formeZoom2 (FormeZoom a b l c d) m n = (Zoom a b l c m n)

formeFile2 :: FormeZoom -> FileInfo
formeFile2(FormeZoom a b l c d) = d