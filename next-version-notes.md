# Towards next version (fixes and stuff that happen between versions)

A majour code refactoring exercise was under taken. Not only was code improved where possible new functionality was added and in a few cases removed.

## Incompatibility Issues:


## Other Work Done:

- Added load-data to before of index lookup methods to make sure data is loaded before trying to lookup indexes.

- Added checking for load-data to prevent endless recursion.


## Bug fixes

- Rolled back query-data refactoring, map-append crashed at 10 mil documents. Reduce does not have the same issues. Its obvious in retrospec,

