# cl-naive-store

cl-naive-store is a simple lazy loading in memory, persistable data store. 

I am hesitant to classify cl-naive-store because by just overriding a couple 
of methods you could make it what ever you want it to be. 

That said the default implementation can loosely be classified as a nosql 
document database. 

**Documentation** can be found on the [wiki](https://gitlab.com/Harag/cl-naive-store/wikis/home)

**Examples** are also on the [wiki](https://gitlab.com/Harag/cl-naive-store/wikis/examples) 
or have a look at the [test.lisp](https://gitlab.com/Harag/cl-naive-store/blob/master/tests/tests.lisp)
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