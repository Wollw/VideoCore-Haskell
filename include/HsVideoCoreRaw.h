#ifndef HSVIDEOCORERAW_H
#define HSVIDEOCORERAW_H

#define FFI_FUNC(ident, type) \
foreign import ccall "ident" ident :: type

#endif
