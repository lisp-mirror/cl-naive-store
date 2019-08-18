# cl-naive-store
**TThis is a naive (simple, not full of bloat), persisted, in memory 
(lazy loading), indexed, data store for Common Lisp.**

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

In summary this db is relatively fast and small in memory (if used right) but 
you have to apply your mind, so use it at your own peril!

For **examples** have a look at the [test.lisp](https://gitlab.com/Harag/cl-naive-store/blob/master/tests/tests.lisp)
it has a lot of comments.

See https://gitlab.com/Harag/cl-wfx to see a very complicated use of 
naive-store.

**Dependencies**

cl-fad
split-sequence
uuid

**Supported CL Implementations**

All Tests pass on SBCL an ECL

**Outstanding stuff (in order of priority):**
- Implement api so remote stores can communitcate with each other.