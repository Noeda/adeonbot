#define _GNU_SOURCE

#include <termios.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int64_t make_pty_and_fork( const char* path, const char** args, int* err, int w, int h, int* masterfd_ptr )
{
    int masterfd = open("/dev/ptmx", O_RDWR|O_CLOEXEC);
    if ( masterfd == -1 ) {
        (*err) = errno;
        return -1;
    }

    char buf[1024];
    memset(buf, 0, 1024);

#ifndef __FreeBSD__
    if ( ptsname_r(masterfd, buf, 1023) == -1 ) {
        (*err) = errno;
        close(masterfd);
        return -1;
    }
#else
    char* nm = ptsname(masterfd);
    strlcpy(buf, nm, 1024);
#endif

    if ( grantpt(masterfd) == -1 ) {
        (*err) = errno;
        close(masterfd);
        return -1;
    }

    if ( unlockpt(masterfd) == -1 ) {
        (*err) = errno;
        close(masterfd);
        return -1;
    }

    int slavefd = open(buf, O_RDWR|O_CLOEXEC);
    if ( slavefd == -1 ) {
        (*err) = errno;
        close(masterfd);
        return -1;
    }

    pid_t pid = fork();
    if ( pid == -1 ) {
        (*err) = errno;
        close(masterfd);
        close(slavefd);
        return -1;
    }

    if ( pid == 0 ) {
        // child
        close(masterfd);
        dup2(slavefd, 0);
        dup2(slavefd, 1);
        dup2(slavefd, 2);
        close(slavefd);

        setsid();
        ioctl(slavefd, TIOCSCTTY, 0);

        struct winsize ws;
        memset( &ws, 0, sizeof(ws) );

        ws.ws_row = h;
        ws.ws_col = w;

        ioctl(slavefd, TIOCSWINSZ, &ws);

        execv( path, (char**) args );
        fprintf(stderr, "execve() failed: %s\n", strerror(errno));
        abort();
    }
    // master
    close(slavefd);

    (*masterfd_ptr) = masterfd;
    return (int64_t) pid;
}

