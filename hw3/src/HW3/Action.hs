module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception)
import Data.Set (Set)
import GHC.IO.Exception (IOErrorType (PermissionDenied))

data HiPermission =
    AllowRead
  | AllowWrite
  deriving (Show, Eq)

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
