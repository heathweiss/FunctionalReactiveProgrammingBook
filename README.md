# FunctionalReactiveProgrammingBook
================================== sodium => reflex =====================================
sodium===================================================reflex==========================
transactioins                                            frames
Event: stream of events                                   Event: stream of occurences


===================================== sodium definitions =======================================
Cells: represent values that change over time, like cells in a spreadsheet
-class in 



life cycle:
-initialization: code statement converted into directed graph
-running: frp engine runs, and can modify the directed graph

Streams: represent streams of events, or a flow of cells

transaction:
A point in time that contains an event.
== frame in Reflex

===================================== reflex definitions =======================================
Event: https://qfpl.io/posts/reflex/basics/events/
data Event t a
Something that has values of a particular type at various instants of time.
And we can think of it as being a bit like a list of pairs of times and values: [(t, a)] 
Is 1 of the 2 main types, other being behavior.


Behavior: https://qfpl.io/posts/reflex/basics/behaviors/
Is something that has a value at all points in time.
A Behavior in reflex looks like this:
 data Behavior t a
 and we can think of it as being like a function from time to values: t -> a
 Is 1 of the 2 main types, other being event.

 Dynamic:
 A combination of an Event and a Behavior that saves past events, and can raise it's own events.

 frame:
 An observable instance of time that contains and Event.
 == transaction in Sodium