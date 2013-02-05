#include <signal.h>
typedef void (*sighandler_t)(int);
void csignal_( int* signum, sighandler_t handler)
{
 signal(*signum, handler);
}
