# cl-naive-store
**This is a naive, persisted, in memory (lazy loading) data store for Common Lisp.**

cl-naive-store is designed so that you can decide how much of the avaible
functionality you want to use. For example cl-naive-store starts out as non
indexed. You need to load cl-naive-indexed to get indexing functionality, 
cl-naive-data-types if you want data-types, and cl-naive-items if you want the
kitchen sink.


cl-naive-store expects data to be proper lists by default. If you dont like 
that its very simple to override, have a look at cl-naive-items code for an
example.

The core (loading and persisting of data) is blissfully unaware of 
"Data Types". You dont have to set up type definitions at all you can just 
throw data at it! Even if you use data types the loading and persisting of data
does not use them at all.

Loading of stuff into memory is delayed as long as possible that goes for data 
and actual structural elements of the data store like type definitions.

The defenitions of stores, collections and data-types are stored in their own
files within the directories used to persist the data. These files helps with 
the lazy loading of stuff. Each collection has its own directory which helps 
with blobs and conceptually splitting up data (see cl-wfx to see how far 
this can be taken).


**Outstanding stuff (in order of priority):**
- Create tests for cl-naive-items.
- Test on lisp other than SBCL so it can be submitted to QuickLisp.
- Implement comms with external data stores, maybe as an additonal package or
something.

In summary this db is relatively fast and small in memory (if used right) but 
you have to apply your mind, so use it at your own peril!

For **examples** have a look at the [test.lisp](https://gitlab.com/Harag/cl-naive-store/blob/master/tests/tests.lisp)
it has a lot of comments.

See https://gitlab.com/Harag/cl-wfx to see a very complicated use of 
naive-store.

**Some Benchmarks: (sbcl instance with 12gigs of memory allocated to it)**

***Indexed Collection:***

Add 100000 objects to collection
Evaluation took:
  0.465 seconds of real time
  0.465624 seconds of total run time (0.455678 user, 0.009946 system)
  [ Run times consist of 0.031 seconds GC time, and 0.435 seconds non-GC time. ]
  100.22% CPU
  1,306,336,804 processor cycles
  392,158,576 bytes consed
  
Persist collection
Evaluation took:
  0.401 seconds of real time
  0.401521 seconds of total run time (0.393466 user, 0.008055 system)
  100.25% CPU
  1,127,037,741 processor cycles
  260,832,912 bytes consed
  
Query collection
Evaluation took:
  0.008 seconds of real time
  0.008111 seconds of total run time (0.008106 user, 0.000005 system)
  100.00% CPU
  22,772,620 processor cycles
  0 bytes consed
  
***Non Inedexed Collection:***

Add 100000 objects to collection
Evaluation took:
  0.032 seconds of real time
  0.031967 seconds of total run time (0.031945 user, 0.000022 system)
  100.00% CPU
  89,752,223 processor cycles
  62,407,600 bytes consed
  
Persist collection
Evaluation took:
  0.299 seconds of real time
  0.299574 seconds of total run time (0.295625 user, 0.003949 system)
  [ Run times consist of 0.037 seconds GC time, and 0.263 seconds non-GC time. ]
  100.33% CPU
  839,970,533 processor cycles
  240,054,624 bytes consed
  
Query moster collection
Evaluation took:
  0.003 seconds of real time
  0.003409 seconds of total run time (0.003409 user, 0.000000 system)
  100.00% CPU
  9,568,315 processor cycles
  1,605,632 bytes consed

