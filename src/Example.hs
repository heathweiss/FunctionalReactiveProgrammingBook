{-# LANGUAGE OverloadedLabels, FlexibleContexts, OverloadedStrings, RecursiveDo, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This is the exmaple that comes with reflex-gi-gtk.
It is used as the basis for building the frp examples using gtk and reflex.
-}
module Example(mainExample) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import qualified Data.Map.Lazy as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Reflex ( Dynamic
              , Event
              , (<@>)
              , current
              , holdDyn
              , list
              , listHoldWithKey
              , mergeList
              , performEvent_
              , switch
              )
import Reflex.GI.Gtk ( ReactiveAttrOp((:==))
                     , MonadReflexGtk
                     , runGtk
                     , runReflexGtk
                     , eventOnSignal
                     , eventOnSignal0
                     , sink
                     , sinkBoxUniform
                     )
import System.Exit ( ExitCode(ExitFailure)
                   , die
                   , exitSuccess
                   , exitWith
                   )
import System.Environment ( getArgs
                          , getProgName
                          )

stringInput :: (MonadReflexGtk t m)
            => m (Gtk.Widget, Dynamic t T.Text, Event t ())
stringInput = do
  ( input
    , deleteButton
    , inputW
    ) <- runGtk $ do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    input <- Gtk.entryNew
    deleteButton <- Gtk.buttonNewFromIconName (Just "list-remove") $
      fromIntegral $ fromEnum Gtk.IconSizeButton
    #packStart box input True True 0
    #packStart box deleteButton False False 0
    #showAll box
    inputW <- Gtk.toWidget box
    pure (input, deleteButton, inputW)
  newTextE <- eventOnSignal input #changed (Gtk.get input #text >>=)
  textDyn <- holdDyn T.empty newTextE
  delete <- eventOnSignal0 deleteButton #clicked
  pure (inputW, textDyn, delete)

-- | Runs the example
mainExample :: IO ()
mainExample = do
  argv <- liftA2 (:) getProgName getArgs
  Gtk.applicationNew (Just "de.weltraumschlangen.reflex-test") []
    >>= maybe
    (die "Failed to initialize GTK")
    ( \application -> do
        ret <- runReflexGtk application (Just argv) $ do
          mainWindow <- runGtk $ Gtk.applicationWindowNew application
          activate <- eventOnSignal0 application #activate

          outerBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 2
          inputBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 0
          outputBox <- Gtk.boxNew Gtk.OrientationVertical 5
          addInputButton <- runGtk $ Gtk.buttonNewFromIconName (Just "list-add") $
            fromIntegral $ fromEnum Gtk.IconSizeButton
          Gtk.set addInputButton [#label Gtk.:= "Add"]
          runGtk $ do
            #add mainWindow outerBox
            #packStart outerBox inputBox False False 0
            #packStart outerBox addInputButton False False 0
            #packStart outerBox outputBox False False 0

          addInput <- eventOnSignal0 addInputButton #clicked

          rec
            let freeKey = maybe (minBound :: Word) (succ . fst) . M.lookupMax
                          <$> current inputWidgets
                inputWidgetUpdates = mconcat
                                     [ (\k -> M.singleton k . Just) <$> freeKey <@> addInput
                                     , M.fromList . map (,Nothing) . NE.toList <$> delete
                                     ]
            inputWidgets <-
              listHoldWithKey (M.singleton 0 ()) inputWidgetUpdates $ \k () -> do
              (\(widget, text, delete') -> (widget, text, k <$ delete')) <$> stringInput

            let delete = switch $ mergeList . map (\(_, _, d) -> d) . M.elems
                         <$> current inputWidgets

          sinkBoxUniform inputBox (M.map (\(w, _, _) -> w) <$> inputWidgets)
            False False 0 Gtk.PackTypeStart

          outputWidgets <- list (M.map (\(_, t, _) -> t) <$> inputWidgets) 
            $ \textBB -> do
            let textB = join textBB
            label <- runGtk $ Gtk.labelNew Nothing
            sink label [#label :== textB]
            #show label
            pure label
          sinkBoxUniform outputBox outputWidgets True True 10 Gtk.PackTypeStart

          performEvent_ $ runGtk (#showAll mainWindow) <$ activate

        case ret of
          0 -> exitSuccess
          n -> exitWith $ ExitFailure $ fromIntegral n
    )
