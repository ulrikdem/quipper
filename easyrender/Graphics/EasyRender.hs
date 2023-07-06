{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides efficient functions for rendering vector
-- graphics to a number of formats, including EPS, PostScript, and
-- PDF. It provides an abstraction for multi-page documents, as well
-- as a set of graphics primitives for page descriptions. 
-- 
-- The graphics model is similar to that of the PostScript and PDF
-- languages, but we only implement a subset of their functionality.
-- Care has been taken that graphics rendering is done efficiently and
-- as lazily as possible; documents are rendered \"on the fly\",
-- without the need to store the whole document in memory.
-- 
-- The provided document description model consists of two separate
-- layers of abstraction:
-- 
-- * /drawing/ is concerned with placing marks on a fixed surface, and
-- takes place in the 'Draw' monad;
-- 
-- * /document structure/ is concerned with a sequence of pages, their
-- bounding boxes, and other meta-data. It takes place in the
-- 'Document' monad.

module Graphics.EasyRender (
  -- * Types
  -- ** Coordinates
  X,
  Y,
  -- ** Color
  Color(..),
  
  -- ** Fonts
  Basefont(..),
  Font(..),
  nominalsize,
  text_width,
  
  -- ** Alignment
  Alignment,
  align_left,
  align_center,
  align_right,
  
  -- * The Document monad
  -- $DOCUMENTMODEL
  Document,
  newpage,
  newpage_defer,
  endpage,
  
  -- * The Draw monad  
  -- $DRAWINGMODEL
  Draw,
  
  -- ** Path construction commands
  -- $PATHCONSTRUCTION
  newpath,
  moveto,
  lineto,
  curveto,
  closepath,
  arc,
  arc_append,
  oval,
  rectangle,
  
  -- ** Painting commands
  stroke,
  fill,
  fillstroke,

  -- ** Clipping path
  clip,
  
  -- ** Text commands
  textbox,
  
  -- ** Graphics parameters
  -- $GRAPHICSPARAMETERS
  setlinewidth,
  setcolor,
  
  -- ** Coordinate system
  -- $COORDINATESYSTEM
  translate,
  scale,
  
  -- ** Comments
  comment,
  
  -- ** Block structure
  -- $BLOCKSTRUCTURE
  block,
  
  -- * Backends
  -- $BACKENDS
  RenderFormat(..),
  render_stdout,
  render_file,
  render_string,
  
  -- * Customization
  -- $CUSTOMIZATION
  
  -- ** Custom drawing commands
  -- $CUSTOMCOMMANDS
  draw_subroutine,
  custom_ps,  
  custom_pdf,
  custom_ascii,

  -- ** Customization interface
  Custom(..),
  custom,
  
  -- ** Customized rendering functions
  -- $CUSTOMRENDER
  render_custom_stdout,
  render_custom_file,
  render_custom_string,
  ) where

import Graphics.EasyRender.Internal

-- $DOCUMENTMODEL 
-- 
-- Document description takes place in the 'Document' monad. A basic
-- multi-page document has the following structure:
-- 
-- > document :: Document ()
-- > document = do
-- >   newpage x y $ do
-- >     <<<drawing commands>>>
-- >   newpage x y $ do
-- >     <<<drawing commands>>>
-- >   ...
-- 
-- Here, each 'newpage' command describes one page of the
-- document. The parameters /x/ and /y/ specify the dimensions of the
-- page bounding box. They are expressed in units of PostScript
-- points, i.e., multiples of 1/72 inch.
-- 
-- Sometimes the bounding box for a page is not known until after the
-- page content has been generated. For this purpose, we also provide
-- the following alternative to the 'newpage' command:
-- 
-- >   newpage_defer $ do
-- >     <<<drawing commands>>>
-- >     endpage x y
-- 
-- It works just like the 'newpage' command, except that the bounding
-- box is given at the end.

-- $DRAWINGMODEL 
-- 
-- The description of the visible content of a page take place in the
-- 'Draw' monad. It takes the form of a sequence of drawing commands,
-- for example:
-- 
-- >     moveto 10 10
-- >     lineto 10 100
-- >     lineto 100 100
-- >     lineto 100 10
-- >     closepath
-- >     stroke
-- 
-- The graphics model is similar to that of the PostScript and PDF
-- languages. The basic concept is that of a /path/, which is a
-- sequence of straight and curved line segments. Paths are first
-- constructed using /path construction commands/, and then painted
-- using /painting commands/, depending on a set of current 
-- /graphics parameters/ and a current /coordinate system/.
-- 
-- We also provide block structure. Changes to the graphics state
-- (color, coordinate system, etc.) that are done within a block are
-- local to the block.
-- 
-- >     block $ do
-- >       <<drawing commands>>

-- $PATHCONSTRUCTION
--   
-- During path construction, there is a notion of /current path/ and
-- /current point/. A path may consist of zero or more connected
-- subpaths, and each subpath is either open or closed. 

-- $GRAPHICSPARAMETERS
-- 
-- The painting commands rely on a set of graphics parameters. The
-- graphics parameters are initially set to default values, and can be
-- altered with the following commands.

-- $COORDINATESYSTEM
-- 
-- Coordinates, lengths, widths, etc, are all interpreted relative to
-- a /current coordinate system/. The initial coordinate system of
-- each page has the origin in the lower left corner, with each unit
-- equalling one PostScript point (1/72 inch). The following commands
-- can be used to change the current coordinate system.

-- $BLOCKSTRUCTURE 
-- 
-- Drawing operations can be grouped into blocks with the 'block'
-- operator. Changes to the graphics parameters and coordinate system
-- are local to the block. It is undefined whether changes to the
-- current path made within a block persist after the end of the block
-- (they do in PDF, but not in PostScript). Therefore, path
-- construction should not be broken up across end-of-block boundaries.

-- $CUSTOMIZATION
-- 
-- The document and drawing abstractions provided by this module are
-- purposely kept general-purpose, and do not include
-- application-specific features. However, we provide a mechanism by
-- which applications can provide customized drawing commands and
-- other custom features.

-- $CUSTOMCOMMANDS
-- 
-- It is sometimes useful to use customized drawing commands. For
-- example, an application that draws many rectangles might like to
-- define a custom 'rectangle' function for appending a rectangle to
-- the current path. Of course this can be defined as an ordinary
-- Haskell function, using elementary drawing commands:
-- 
-- > my_rect :: X -> Y -> X -> Y -> Draw ()
-- > my_rect x0 y0 x1 y1 = do
-- >   moveto x0 y0
-- >   lineto x0 y1
-- >   lineto x1 y1
-- >   lineto x1 y0
-- >   closepath
-- 
-- However, sometimes it is nice to make use of specialized abilities
-- of individual backends. For example, PDF already has a built-in
-- rectangle drawing command, and PostScript has the ability to define
-- custom subroutines within the document text. Using these features
-- can decrease the size of the generated documents. 
-- 
-- We therefore provide a facility for defining new drawing commands
-- with backend-specific implementations. For example, a more general
-- version of the above 'my_rect' function can be defined as
-- follows:
-- 
-- > my_rect :: X -> Y -> X -> Y -> Draw ()
-- > my_rect x0 y0 x1 y1 = draw_subroutine alt $ do
-- >   moveto x0 y0
-- >   lineto x0 y1
-- >   lineto x1 y1
-- >   lineto x1 y0
-- >   closepath
-- >  where
-- >   alt = [
-- >     custom_ps $      printf "%f %f %f %f rect\n" x0 y0 x1 y1,
-- >     custom_pdf $     printf "%f %f %f %f re\n" x0 y0 (x1-x0) (y1-y0),
-- >     custom_ascii $   printf "My_rect %f %f %f %f\n" x0 y0 x1 y1
-- >     ]
-- 
-- The idea is to provide a default definition in terms of primitive
-- drawing commands, as well as a list of various backend specific
-- definitions. In the case of PostScript subroutines, the PostScript
-- file must then also be supplied with a definition for the /rect/
-- subroutine, which can be done with the command 'render_ps_custom':
-- 
-- > my_ps_defs = "/rect { ... } bind def\n"
-- >
-- > my_render_ps = render_ps_custom custom { ps_defs = my_ps_defs }
-- 
-- Note that the 'draw_subroutine' customization mechanism is entirely
-- optional. Its purpose is to generate shorter output for some
-- backends; if it is omitted, the file may be be longer but should
-- look the same.

-- $BACKENDS 
-- 
-- The following commands can be used to render documents to various
-- available formats. The available formats are PostScript, PDF, EPS,
-- and an ASCII-based debugging format. Output can be written to
-- standard output, to a file, or to a string.
  
-- $CUSTOMRENDER
-- 
-- The following are versions of the generic rendering functions that
-- also take a customization data structure as an additional
-- parameter.

