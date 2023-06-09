-- | Functions to decompose circuits into various gate bases.

module Quipper.Libraries.Decompose ( 
  -- * Precision
  Precision,
  bits,
  digits,
  -- * Phase
  KeepPhase,
  -- * Gate bases
  GateBase (..),
  gatebase_enum,
  -- * Generic decomposition
  decompose_generic,
) where

import Quipper.Libraries.Decompose.CliffordT
import Quipper.Libraries.Decompose.GateBase
import Quipper.Libraries.Decompose.Legacy
import Quipper.Libraries.Synthesis
