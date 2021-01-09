#include <cstdint>
// tls.cpp
thread_local int64_t tls_value;
int64_t tls_func() {
    return tls_value;
}
