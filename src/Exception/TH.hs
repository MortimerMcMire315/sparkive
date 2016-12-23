{-# LANGUAGE QuasiQuotes #-}

module Exception.TH 
     ( buildErrorHandler
     ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (quoteDec)
import Data.Maybe (fromJust)
import Data.Typeable.Internal (Typeable)
import Control.Exception (Exception)

{-|
   Given a name, i.e. ConfigParseException,
   - build a simple data type: data ConfigParseException String
   - make the data type an instance of Exception
   - create a handler function returning (m (Either String a)) with an appropriate type signature
-}
buildErrorHandler :: String -> Q [Dec]
buildErrorHandler s = do
    conName     <- newName s
    typeName    <- newName s
    handlerName <- newName ("handle" ++ s)

    dDec  <- dataDec conName typeName
    iDec  <- instanceDec typeName
    hFSig <- handlerFSig handlerName
    hFDec <- handlerFDec conName handlerName

    return [dDec, iDec, hFSig, hFDec]

handlerFDec :: Name -> Name -> Q Dec
handlerFDec conName handlerName = do
    maybeHandler <- lookupValueName "Handler"
    maybeLeftN   <- lookupValueName "Left"
    maybeReturn  <- lookupValueName "return"
    handler      <- return $ fromJust maybeHandler
    leftN        <- return $ fromJust maybeLeftN
    returnN      <- return $ fromJust maybeReturn

    lambdaVar <- newName "str"
    return $ FunD 
        handlerName 
        [Clause 
            []
            (NormalB $ AppE 
                         (ConE handler) $
                         (LamE 
                            [ConP conName [VarP lambdaVar]] $
                            AppE (VarE returnN) $ AppE (ConE leftN) (VarE lambdaVar)
                         )
            )
            []
        ]

handlerFSig :: Name -> Q Dec
handlerFSig handlerName = do
    maybeHandlerT   <- lookupTypeName  "Handler"
    maybeMonadCatch <- lookupTypeName  "MonadCatch"
    maybeMonadThrow <- lookupTypeName  "MonadThrow"
    maybeEither     <- lookupTypeName  "Either"
    maybeString     <- lookupTypeName  "String"
    handlerT   <- return $ fromJust maybeHandlerT
    monadCatch <- return $ fromJust maybeMonadCatch
    monadThrow <- return $ fromJust maybeMonadThrow
    eitherN    <- return $ fromJust maybeEither
    string     <- return $ fromJust maybeString

    monadTypeVar <- newName "m"
    anyTypeVar   <- newName "a"

    return $ SigD handlerName $ ForallT [PlainTV monadTypeVar, PlainTV anyTypeVar]
                --Class constraints (m must be an instance of MonadCatch and MonadThrow)
                ([AppT (ConT monadCatch) (VarT monadTypeVar), AppT (ConT monadThrow) (VarT monadTypeVar)]) $ --Class constraints
                -- Handler m (Either String a)
                AppT (AppT (ConT handlerT) (VarT monadTypeVar)) $ AppT (AppT (ConT eitherN) $ ConT string) $ VarT anyTypeVar

instanceDec :: Name -> Q Dec
instanceDec typeName = do
    maybeException  <- lookupTypeName  "Exception"
    exception  <- return $ fromJust maybeException

    return $ InstanceD 
                Nothing 
                [] 
                (AppT (ConT exception) (ConT typeName))
                []

dataDec :: Name -> Name -> Q Dec
dataDec conName typeName = do
    maybeTypeable   <- lookupTypeName  "Typeable"
    maybeShow       <- lookupTypeName  "Show"
    maybeString     <- lookupTypeName  "String"
    typeable   <- return $ fromJust maybeTypeable
    show       <- return $ fromJust maybeShow
    string     <- return $ fromJust maybeString

    let nobang = Bang NoSourceUnpackedness NoSourceStrictness
    let typecon = [NormalC conName [(nobang, ConT string)]]
    return $ DataD [] typeName [] Nothing typecon [ConT show, ConT typeable]
