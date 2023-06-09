-- | This library provides functions for simulating certain classes of
-- circuits, for testing and debugging purposes. 
-- 
-- We can efficiently simulate classical (boolean) circuits and
-- Clifford (stabilizer) circuits. We also provide functions for
-- simulating arbitrary quantum circuits; however, the latter is
-- (necessarily) very inefficient.

module Quipper.Libraries.Simulation ( 
  -- * Classical simulation
  run_classical_generic,
  
  -- * Stabilizer simulation
  run_clifford_generic,
  
  -- * Quantum simulation
  run_generic,
  run_generic_io,
  sim_amps,
  
  -- * Special purpose functions
  -- ** Simulation with trace
  QuantumTrace (..),
  run_generic_trace,
  run_generic_trace_io,
  
  -- ** Probability distributions
  Vector (..),
  ProbabilityDistribution (..),
  sim_generic,
) where

import Quipper.Libraries.Simulation.ClassicalSimulation
import Quipper.Libraries.Simulation.QuantumSimulation
import Quipper.Libraries.Simulation.CliffordSimulation

