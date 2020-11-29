
module Data.VarTable
    ( VarTable
    , new
    , inherit
    , isGlobal
    , copy
    , insert
    , lookup
    , lookupWithFallback
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef          (IORef, modifyIORef', newIORef, readIORef)
import           Data.Maybe          (isNothing)
import           Data.Text           (Text)
import           Prelude             hiding (lookup)

data VarTable a = VarTable
    { _vt_global   :: Maybe (VarTable a)
    , _vt_bindings :: IORef (HashMap Text a)
    }

new :: IO (VarTable a)
new = VarTable Nothing <$> newIORef HashMap.empty

inherit :: VarTable a -> IO (VarTable a)
inherit vt@(VarTable global bindings) = do
    let global' = case global of
                    Nothing -> Just vt
                    _       -> global
    bindings' <- newIORef =<< readIORef bindings
    pure $ VarTable global' bindings'

isGlobal :: VarTable a -> Bool
isGlobal vt = isNothing (_vt_global vt)

copy :: VarTable a -> (a -> IO a) -> IO (VarTable a)
copy vt vc = do
    global' <- case _vt_global vt of
                 Nothing -> pure Nothing
                 Just t  -> Just <$> copy t vc
    bindings' <- newIORef =<< traverse vc =<< readIORef (_vt_bindings vt)
    pure $ VarTable global' bindings'

insert :: Text -> a -> VarTable a -> IO ()
insert key r vt =
    modifyIORef' (_vt_bindings vt) (HashMap.insert key r)

lookup :: Text -> VarTable a -> IO (Maybe a)
lookup key vt =
    HashMap.lookup key <$> readIORef (_vt_bindings vt) >>= \case
        Just x  -> pure $ Just x
        Nothing ->
            case _vt_global vt of
              Just x  -> lookup key x
              Nothing -> pure Nothing

lookupWithFallback :: Text -> IO a -> VarTable a -> IO a
lookupWithFallback key mkvar vt =
    HashMap.lookup key <$> readIORef (_vt_bindings vt) >>= \case
        Just x  -> pure x
        Nothing ->
            case _vt_global vt of
              Just x  -> lookupWithFallback key mkvar x
              Nothing -> do
                  r <- mkvar
                  insert key r vt
                  pure r
