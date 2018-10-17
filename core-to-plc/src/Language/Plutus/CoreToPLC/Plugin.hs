{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module Language.Plutus.CoreToPLC.Plugin (PlcCode, getSerializedCode, applyPlc, getAst, plugin, plc, lifted) where

import           Language.Plutus.CoreToPLC.Compiler.Builtins
import           Language.Plutus.CoreToPLC.Compiler.Expr
import           Language.Plutus.CoreToPLC.Compiler.Types
import           Language.Plutus.CoreToPLC.Compiler.Utils
import           Language.Plutus.CoreToPLC.Error
import           Language.Plutus.Lift

import qualified GhcPlugins                                  as GHC
import qualified Panic                                       as GHC

import qualified Language.PlutusCore                         as PLC
import           Language.PlutusCore.Quote

import           Language.Haskell.TH.Syntax                  as TH

import           Codec.Serialise                             (DeserialiseFailure, Serialise, deserialiseOrFail,
                                                              serialise)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson                                  (FromJSON (parseJSON), ToJSON (toJSON), withText)
import qualified Data.Aeson                                  as JSON
import           Data.Bifunctor                              (first)
import qualified Data.ByteString.Base64                      as Base64
import qualified Data.ByteString.Lazy                        as BSL
import qualified Data.Map                                    as Map
import           Data.Maybe                                  (catMaybes)
import qualified Data.Text.Encoding                          as TE
import qualified Data.Text.Prettyprint.Doc                   as PP
import           GHC.TypeLits

{- Note [Constructing the final program]
Our final type is a simple newtype wrapper. However, constructing *anything* in Core
is a pain - we have to go off and find the right constructor, ensure we've applied it
correctly etc. But since it *is* just a wrapper... we can just put in a coercion!

Very nice and easy, but we need to make sure we don't stop being a simple newtype
without revisiting this.

We also obviously don't want to break anyone by changing the internals, so the type
should be abstract.
-}

-- See Note [Constructing the final program]
-- | A PLC program.
newtype PlcCode = PlcCode { unPlc :: [Word] }
  --  The encoding generated by deriving Serialise is the same as getSerializedCode except that it is surrounded by TkListBegin and TkBreak Tokens
  deriving newtype Serialise

-- Note that we do *not* have a TypeablePlc instance, since we don't know what the type is. We could in principle store it after the plugin
-- typechecks the code, but we don't currently.
instance LiftPlc PlcCode where
    lift (getAst -> (PLC.Program () _ body)) = PLC.globalRename body

instance ToJSON PlcCode where
  toJSON = JSON.String . TE.decodeUtf8 . Base64.encode . BSL.toStrict . serialise

instance FromJSON PlcCode where
  parseJSON = withText "PlcCode" $ \s -> do
    let ev = do
          eun64 <- Base64.decode . TE.encodeUtf8 $ s
          first show $ deserialiseOrFail $ BSL.fromStrict eun64
    case ev of
      Left e  -> fail e
      Right v -> pure v

lifted :: LiftPlc a => a -> PlcCode
lifted a =
    let term = runQuote $ Language.Plutus.Lift.lift a
        program = PLC.Program () (PLC.defaultVersion ()) term
        serialized = serialise program
    in
        PlcCode $ fromIntegral <$> BSL.unpack serialized

getSerializedCode :: PlcCode -> BSL.ByteString
getSerializedCode = BSL.pack . fmap fromIntegral . unPlc

-- | Apply a function to an argument in PLC
applyPlc :: PlcCode -> PlcCode -> PlcCode
applyPlc (getAst -> f) (getAst -> x) = PlcCode words' where
    program = f `PLC.applyProgram` x
    serialized = serialise program
    words' = fromIntegral <$> BSL.unpack serialized

{- Note [Deserializing the AST]
The types suggest that we can fail to deserialize the AST that we embedded in the program.
However, we just did it ourselves, so this should be impossible, and we signal this with an
exception.
-}
newtype ImpossibleDeserialisationFailure = ImpossibleDeserialisationFailure DeserialiseFailure
instance Show ImpossibleDeserialisationFailure where
    show (ImpossibleDeserialisationFailure e) = "Failed to deserialise our own program! This is a bug, please report it. Caused by: " ++ show e
instance Exception ImpossibleDeserialisationFailure

getAst :: PlcCode -> PLC.Program PLC.TyName PLC.Name ()
getAst wrapper = case deserialiseOrFail $ getSerializedCode wrapper of
    Left e  -> throw $ ImpossibleDeserialisationFailure e
    Right p -> p

-- | Marks the given expression for conversion to PLC.
plc :: forall (loc::Symbol) a . a -> PlcCode
-- this constructor is only really there to get rid of the unused warning
plc _ = PlcCode mustBeReplaced

data PluginOptions = PluginOptions {
    poDoTypecheck   :: Bool
    , poDeferErrors :: Bool
    }

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.installCoreToDos = install }

install :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
install args todo =
    let
        opts = PluginOptions {
            poDoTypecheck = notElem "dont-typecheck" args
            , poDeferErrors = elem "defer-errors" args
            }
    in
        pure (GHC.CoreDoPluginPass "Core to PLC" (pluginPass opts) : todo)

pluginPass :: PluginOptions -> GHC.ModGuts -> GHC.CoreM GHC.ModGuts
pluginPass opts guts = qqMarkerName >>= \case
    -- nothing to do
    Nothing -> pure guts
    Just name -> GHC.bindsOnlyPass (mapM $ convertMarkedExprsBind opts name) guts

{- Note [Hooking in the plugin]
Working out what to process and where to put it is tricky. We are going to turn the result in
to a 'PlcCode', not the Haskell expression we started with!

Currently we look for calls to the 'plc :: a -> PlcCode' function, and we replace the whole application with the
generated code object, which will still be well-typed.

However, if we do this with a polymorphic expression as the argument to 'plc', we have problems
where GHC gives unconstrained type variables the type `Any` rather than leaving them abstracted as we require (see
note [System FC and system FW]). I don't currently know how to resolve this.
-}

qqMarkerName :: GHC.CoreM (Maybe GHC.Name)
qqMarkerName = GHC.thNameToGhcName 'plc

qqMarkerType :: GHC.Type -> Maybe GHC.Type
qqMarkerType vtype = do
    (_, ty) <- GHC.splitForAllTy_maybe vtype
    (_, ty') <- GHC.splitForAllTy_maybe ty
    (_, o) <- GHC.splitFunTy_maybe ty'
    pure o

makePrimitiveMap :: [(TH.Name, a)] -> GHC.CoreM (Map.Map GHC.Name a)
makePrimitiveMap associations = do
    mapped <- forM associations $ \(name, term) -> do
        ghcNameMaybe <- GHC.thNameToGhcName name
        pure $ fmap (\ghcName -> (ghcName, term)) ghcNameMaybe
    pure $ Map.fromList (catMaybes mapped)

-- | Converts all the marked expressions in the given binder into PLC literals.
convertMarkedExprsBind :: PluginOptions -> GHC.Name -> GHC.CoreBind -> GHC.CoreM GHC.CoreBind
convertMarkedExprsBind opts markerName = \case
    GHC.NonRec b e -> GHC.NonRec b <$> convertMarkedExprs opts markerName e
    GHC.Rec bs -> GHC.Rec <$> mapM (\(b, e) -> (,) b <$> convertMarkedExprs opts markerName e) bs

-- | Converts all the marked expressions in the given expression into PLC literals.
convertMarkedExprs :: PluginOptions -> GHC.Name -> GHC.CoreExpr -> GHC.CoreM GHC.CoreExpr
convertMarkedExprs opts markerName =
    let
        conv = convertMarkedExprs opts markerName
        convB = convertMarkedExprsBind opts markerName
    in \case
      -- the ignored argument is the type for the polymorphic 'plc'
      e@(GHC.App(GHC.App (GHC.App (GHC.Var fid) (GHC.Type (GHC.isStrLitTy -> Just fs_locStr))) (GHC.Type _)) inner) | markerName == GHC.idName fid ->
          let
              vtype = GHC.varType fid
              locStr = show fs_locStr
          in case qqMarkerType vtype of
              Just t -> convertExpr opts locStr inner t
              Nothing -> do
                  GHC.errorMsg $ "plc Plugin: found invalid marker, could not decode type:" GHC.$+$ GHC.ppr vtype
                  pure e
      e@(GHC.Var fid) | markerName == GHC.idName fid -> do
            GHC.errorMsg "plc Plugin: found invalid marker, not applied correctly"
            pure e
      GHC.App e a -> GHC.App <$> conv e <*> conv a
      GHC.Lam b e -> GHC.Lam b <$> conv e
      GHC.Let bnd e -> GHC.Let <$> convB bnd <*> conv e
      GHC.Case e b t alts -> do
            e' <- conv e
            let expAlt (a, bs, rhs) = (,,) a bs <$> conv rhs
            alts' <- mapM expAlt alts
            pure $ GHC.Case e' b t alts'
      GHC.Cast e c -> flip GHC.Cast c <$> conv e
      GHC.Tick t e -> GHC.Tick t <$> conv e
      e@(GHC.Coercion _) -> pure e
      e@(GHC.Lit _) -> pure e
      e@(GHC.Var _) -> pure e
      e@(GHC.Type _) -> pure e

-- | Actually invokes the Core to PLC compiler to convert an expression into a PLC literal.
convertExpr :: PluginOptions -> String -> GHC.CoreExpr -> GHC.Type -> GHC.CoreM GHC.CoreExpr
convertExpr opts locStr origE resType = do
    flags <- GHC.getDynFlags
    primTerms <- makePrimitiveMap builtinTermAssociations
    primTys <- makePrimitiveMap builtinTypeAssociations
    let result = withContextM (sdToTxt $ "Converting expr at" GHC.<+> GHC.text locStr) $ do
              converted <- convExprWithDefs origE
              when (poDoTypecheck opts) $ do
                  annotated <- convertErrors (NoContext . PLCError) $ PLC.annotateTerm converted
                  void $ convertErrors (NoContext . PLCError) $ PLC.typecheckTerm (PLC.TypeCheckCfg 1000 $ PLC.TypeConfig True mempty) annotated
              pure converted
        context = ConvertingContext {
            ccOpts=ConversionOptions { coCheckValueRestriction=poDoTypecheck opts },
            ccFlags=flags,
            ccPrimTerms=primTerms,
            ccPrimTypes=primTys,
            ccScopes=initialScopeStack
            }
        initialState = ConvertingState Map.empty Map.empty
    case runConverting context initialState result of
        Left s ->
            let shown = show $ PP.pretty s in
            if poDeferErrors opts
            -- TODO: is this the right way to do either of these things?
            then pure $ GHC.mkRuntimeErrorApp GHC.rUNTIME_ERROR_ID resType shown -- this will blow up at runtime
            else liftIO $ GHC.throwGhcExceptionIO (GHC.ProgramError shown) -- this will actually terminate compilation
        Right term -> do
            let program = PLC.Program () (PLC.defaultVersion ()) term
            let serialized = serialise program
            -- The GHC api only exposes a way to make literals for Words, not Word8s, so we need to convert them
            let (word8s :: [Word]) = fromIntegral <$> BSL.unpack serialized
            -- The flags here are so GHC can check whether the word is in range for the current platform.
            -- This will never actually be a problem for us, since they're really Word8s, but GHC
            -- doesn't know that.
            let (wordExprs :: [GHC.CoreExpr]) = fmap (GHC.mkWordExprWord flags) word8s
            let listExpr = GHC.mkListExpr GHC.wordTy wordExprs
            -- See Note [Constructing the final program]
            pure $ GHC.Cast listExpr $ GHC.mkRepReflCo resType
