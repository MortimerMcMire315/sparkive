module Exception.TH
     ( buildErrorHandler
     ) where

import Language.Haskell.TH.Syntax --Pretty much every function call comes from here.
import Language.Haskell.TH.Quote ( quoteDec   )
import Data.Maybe                ( fromJust   )
import Data.Typeable.Internal    ( Typeable   )
import Control.Exception         ( Exception  )

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
    let handler  =  fromJust maybeHandler
    let leftN    =  fromJust maybeLeftN
    let returnN  =  fromJust maybeReturn

    lambdaVar <- newName "str"
    return $ FunD
        handlerName
        [Clause
            []
            (NormalB $ AppE
                         (ConE handler)
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
    let handlerT    =  fromJust maybeHandlerT
    let monadCatch  =  fromJust maybeMonadCatch
    let monadThrow  =  fromJust maybeMonadThrow
    let eitherN     =  fromJust maybeEither
    let string      =  fromJust maybeString

    monadTypeVar <- newName "m"
    anyTypeVar   <- newName "a"

    return $ SigD handlerName $ ForallT [PlainTV monadTypeVar, PlainTV anyTypeVar]
                --Class constraints (m must be an instance of MonadCatch and MonadThrow)
                [AppT (ConT monadCatch) (VarT monadTypeVar), AppT (ConT monadThrow) (VarT monadTypeVar)] $ --Class constraints
                -- Handler m (Either String a)
                AppT (AppT (ConT handlerT) (VarT monadTypeVar)) $ AppT (AppT (ConT eitherN) $ ConT string) $ VarT anyTypeVar

instanceDec :: Name -> Q Dec
instanceDec typeName = do
    maybeException <- lookupTypeName  "Exception"
    let exception  = fromJust maybeException

    return $ InstanceD
                Nothing
                []
                (AppT (ConT exception) (ConT typeName))
                []

dataDec :: Name -> Name -> Q Dec
dataDec conName typeName = do
    maybeTypeable <- lookupTypeName  "Typeable"
    maybeShow     <- lookupTypeName  "Show"
    maybeString   <- lookupTypeName  "String"
    let typeable  = fromJust maybeTypeable
    let show      = fromJust maybeShow
    let string    = fromJust maybeString

    let nobang    = Bang NoSourceUnpackedness NoSourceStrictness
    let typecon   = [NormalC conName [(nobang, ConT string)]]
    return $ DataD [] typeName [] Nothing typecon [ConT show, ConT typeable]
