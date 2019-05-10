# cl-naive-store
This is a naive but cleverish hierarchical persisted in memory data store for Common Lisp.

Naive in that it stores data as proper lists, making files human readable. (There
is nothing worse than to try and find a corruption in your db when the files 
are all gibberish. Besides disk space is cheap.)

Cleverish in that it will try to load only the stuff that is needed into memory.
To do the clever bits the data for the same collection is saved into different files
based on the user setup and the data in the items. (So for example if you have 
multiple companies in your db and a user may only see some companies then only 
those can be loaded when the user signs onto your system.) 

Loading of stuff into memory is delayed as long as possible that goes for data 
and actual structural elements of the data store like type definitions.

Hierarchical because a field of a data-type can reference another item(s) in the 
same store or even a different store. Not all data types have their own collections 
and physical files such sub/child data-types are stored in their parent item's 
collection. (Hopefully this will be extended to external (not in the same image)
data stores in the near future.)

The defenitions of stores, collections and data-types are stored in their own files
within the directories used to persist the data. These files helps with the lazy 
loading of stuff.

Outstanding stuff (in order of priority):
- Do some file locking to protect against basic corruption.
- Implement comms with external data stores, maybe as an additonal package or
something.
- Update testing.lisp to work again.
- Test on lisp other than SBCL so it can be submitted to QuickLisp.

In summary this db is relatively fast and small in memory (if used right) but 
you have to apply your mind, so use it at your own peril!

See https://github.com/Harag/cl-wfx to see a very complicated use of naive-store.
