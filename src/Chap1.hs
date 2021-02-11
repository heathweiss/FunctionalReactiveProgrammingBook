{-# LANGUAGE OverloadedLabels, FlexibleContexts, OverloadedStrings, RecursiveDo, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- 
Uses 'mainCalendarWithEvents' to recreate section 1.12 of Chapter 1
|-}
module Chap1() where

import qualified Data.Text as T
import Control.Applicative (liftA2)
import qualified GI.Gtk as Gtk
import GI.Gtk
import GHC.Int (Int32)
import System.Exit ( ExitCode(ExitFailure)
                   , die
                   , exitSuccess
                   , exitWith
                   )
import System.Environment ( getArgs
                          , getProgName
                          )
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
              , leftmost
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
--attempt to create fx: createNewCalendar outside of the monad. 
--import Reflex.GI.Gtk.Host (ReflexGtk)
--import Reflex.Spider.Internal (SpiderTimeline, SpiderHost)


{- |
Sections 1.12

Recreate the departure and return date picker, for booking flights.
It validates that the dep month is <= return month. Ignores years and days for simplicity.

Only uses Events. Did not see a reason to use Behaviors or Dynamics at this point.
-}
mainCalendarWithEvents :: IO ()
mainCalendarWithEvents = do
  argv <- liftA2 (:) getProgName getArgs
  Gtk.applicationNew (Just "de.weltraumschlangen.reflex-test") []
    >>= maybe
    (die "Failed to initialize GTK")
    ( \application -> do
      ret <- runReflexGtk application (Just argv) $ do
        mainWindow <- runGtk $ Gtk.applicationWindowNew application 
        activate <- eventOnSignal0 application #activate
        outerBox <- runGtk $ Gtk.boxNew Gtk.OrientationVertical 2

        let
          createNewCalendar = runGtk Gtk.calendarNew 
        --print "show me the monad"
        calendarDeparture <- createNewCalendar
        calendarReturn <- createNewCalendar

        --set initial labels on loading
        depMonthLabel <- newMonthLabel calendarDeparture
        retMonthLabel <- newMonthLabel calendarReturn
        isValidLabel <- newValidLabel calendarDeparture calendarReturn

        
        runGtk $ do
            #add mainWindow outerBox
            #packStart outerBox calendarDeparture False False 0
            #packStart outerBox calendarReturn False False 0

            #packStart outerBox depMonthLabel False False 0
            #packStart outerBox retMonthLabel False False 0
            #packStart outerBox isValidLabel False False 0
        
        --handle text display of month changes
        changedDepMonthE <- createChangedMonthEvent calendarDeparture
        createLabelSink depMonthLabel changedDepMonthE
        changedRetMonthE <- createChangedMonthEvent calendarReturn
        createLabelSink retMonthLabel changedRetMonthE
        
        --handle validity checking if dep or return months change
        validateChangedDepMonthE <- newChangedMonthValidationE calendarDeparture calendarDeparture calendarReturn
        validateChangedRetMonthE <- newChangedMonthValidationE calendarReturn    calendarDeparture calendarReturn
        createLabelSink isValidLabel validateChangedDepMonthE
        createLabelSink isValidLabel validateChangedRetMonthE

        performEvent_ $ runGtk (#showAll mainWindow) <$ activate
      case ret of
          0 -> exitSuccess
          n -> exitWith $ ExitFailure $ fromIntegral n   
    )

showT :: (Show a ) => a -> T.Text
showT showMe = T.pack $ show showMe

showDepMonth :: Int32 -> T.Text
showDepMonth month = "Depart: " <> showT (month + 1)

showRetMonth :: Int32 -> T.Text
showRetMonth month =  "Return: " <> showT (month + 1)

validBoolToText :: Int32 -> Int32 ->  T.Text
validBoolToText departMonth returnMonth = 
  if departMonth <= returnMonth 
    then "valid"
    else  "inValid"

getMonthFromCalender  calendar = Gtk.get calendar #month


newMonthLabel calendar = do
   initalDepartureMonth <- getMonthFromCalender calendar
   runGtk $  Gtk.labelNew $ Just $ showDepMonth initalDepartureMonth

--changedDepMonthTextE <- eventOnSignal calendarDeparture #monthChanged ((Gtk.get calendarDeparture #month >>=) . (. showDepMonth))
createChangedMonthEvent calendar = 
  eventOnSignal calendar #monthChanged ((Gtk.get calendar #month >>=) . (. showDepMonth))

--sink showDepMonthLabel [#label :==  changedDepMonthTextE] 
createLabelSink monthLabel changedMonthEvent = sink monthLabel [#label :==  changedMonthEvent] 

newValidLabel calendarDep calendarRet = do
  initalDepartureMonth <- getMonthFromCalender calendarDep
  initalReturnMonth <- getMonthFromCalender calendarRet
  runGtk $  Gtk.labelNew $ Just $ validBoolToText initalDepartureMonth initalReturnMonth

validateMonths calendarDep calendarRet  = do
  depMonth <- Gtk.get calendarDep #month
  retMonth <- Gtk.get calendarRet #month
  return $ validBoolToText depMonth retMonth

newChangedMonthValidationE changedCalendar calendarDep calendarRet = 
  eventOnSignal changedCalendar #monthChanged (validateMonths calendarDep calendarRet >>=)

--can't get the type signature right, and won't compile due to ambigous return type.
--createNewCalendar :: Reflex.GI.Gtk.Host.ReflexGtk  (SpiderTimeline x) (SpiderHost x) Calendar
--createNewCalendar = runGtk Gtk.calendarNew

