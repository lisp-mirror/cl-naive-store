# Version 2020.07.16

A majour code refactoring exercise was under taken and backward compatibility was mostly abandoned. Sorry if that hurts you, it hurts me more I have a lot of projects to update now, but the changes where desperately needed.

When updating the examples 2 to 3 text replaces fixed the combatibility issues so it should not be to bad since 99% of the old expose api was for implementors and not users their should be very little pain.

Implmentors api's where moved to seperate packages to limit the public api.

## Incompatibility Issues:

Only listing the public/user api issues in broad strokes. Thousands of lines of code was changed,deleted or replaced so I list of changes is not really practical.

data-items was renamed to documents
data-types where renamed to document-types
add-data-object was renamed to add-document
persist-data-object was renamed to persist-document
data-type was renamed to document-type
field war renamed to element


The implementor's api was mostly rewritten.

## Other Work Done:


## Bug fixes

