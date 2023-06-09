-- | Platform dependent previewing of PDF files. On Windows, we try to
-- run Acrobat Reader. On Mac, we try to run Preview. On Linux, we try
-- to run Acrobat Reader or Xpdf. If this fails, we abort the program
-- with a platform dependent error message.

module Quipper.Utils.Preview where

import Control.Exception
import System.Environment
import System.Exit
import System.Info
import System.IO
import System.IO.Error
import System.Process

-- | Sequentially try one or more IO computations, until one of them
-- completes without an IO error. If all of them fail, re-throw the
-- error from the last one.
try_several_IO :: [IO a] -> IO a
try_several_IO [] = error "try_several_IO"
try_several_IO [h] = do
  catchIOError h (\e -> throw e)
try_several_IO (h:t) = do
  catchIOError h (\e -> try_several_IO t)

-- | @'system_pdf_viewer' zoom pdffile@: Call a system-specific PDF
-- viewer on /pdffile/ file. The /zoom/ argument is out of 100 and may
-- or may not be ignored by the viewer.
system_pdf_viewer :: Double -> String -> IO ()
system_pdf_viewer zoom pdffile = do
  envList <- getEnvironment
  if (elem ("OS", "Windows_NT") envList) then
    windows_pdf_viewer zoom pdffile
    else if (os == "darwin") then
    macos_pdf_viewer zoom pdffile
    else
    linux_pdf_viewer zoom pdffile

-- | Like 'system_pdf_viewer', but specialized to Windows.
windows_pdf_viewer :: Double -> String -> IO ()
windows_pdf_viewer zoom pdffile = catchIOError body handler
  where
    body = do
      r <- system ("start /WAIT AcroRd32 " ++ pdffile)
      case r of
        ExitSuccess -> return ()
        ExitFailure n -> acrord32_error n
    handler e = do
      name <- getProgName
      hPutStrLn stderr $ name ++ ": unable to preview PDF file. Please ensure that Acrobat Reader is installed."
      exitFailure
    acrord32_error n = do
      name <- getProgName
      hPutStrLn stderr $ name ++ ": unable to preview PDF file. Please ensure that Acrobat Reader is installed."
      exitFailure
      

-- | Like 'system_pdf_viewer', but specialized to Mac OS.
macos_pdf_viewer :: Double -> String -> IO ()
macos_pdf_viewer zoom pdffile = catchIOError body handler
  where
    body = do
      rawSystem "open" [pdffile]
      rawSystem "sleep" ["1"] -- required or the file may be deleted too soon
      return ()
    handler e = do
      name <- getProgName
      hPutStrLn stderr $ name ++ ": unable to preview PDF file. There seems to be a problem with Preview."
      exitFailure
      
-- | Like 'system_pdf_viewer', but specialized to Linux.
linux_pdf_viewer :: Double -> String -> IO ()
linux_pdf_viewer zoom pdffile = catchIOError body handler
  where
    body = do
      try_several_IO [
        rawSystem "acroread" ["/a", "zoom=" ++ show zoom, pdffile],
        rawSystem "xpdf" ["-z", show zoom, pdffile]
        ]
      return ()
    handler e = do
      name <- getProgName
      hPutStrLn stderr $ name ++ ": unable to preview PDF file. Please ensure that either Acrobat Reader or Xpdf are installed."
      exitFailure
