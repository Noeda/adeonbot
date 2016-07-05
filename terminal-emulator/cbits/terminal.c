#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <termios.h>
#include <string.h>
#include <pty.h>

void free_err_msg( char* errmsg )
{
    free(errmsg);
}

void get_terminal_size( int* w, int* h )
{
    (*w) = 80;
    (*h) = 24;

    struct winsize ws;
    memset( &ws, 0, sizeof(struct winsize) );

    if ( ioctl( STDIN_FILENO, TIOCGWINSZ, &ws ) == -1 ) {
        return;
    }

    (*w) = ws.ws_col;
    (*h) = ws.ws_row;
}

struct termios* set_character_mode( char** errmsg )
{
    struct termios to;
    struct termios* old = calloc(sizeof(to), 1);

    if ( !old ) {
        fprintf(stderr, "Out of memory.\n");
        abort();
    }

    if ( tcgetattr( STDIN_FILENO, &to ) == -1 ) {
        if ( asprintf(errmsg, "tcgetattr() failed: %d", errno ) == -1 ) {
            fprintf(stderr, "Out of memory.\n");
            abort();
        }
        free( old );
        return NULL;
    }

    memcpy( old, &to, sizeof(to) );

    to.c_lflag &= ~ECHO;
    to.c_lflag &= ~ISIG;
    to.c_lflag &= ~ICANON;

    if ( tcsetattr( STDIN_FILENO, TCSADRAIN, &to ) == -1 ) {
        char buf[1000];
        if ( strerror_r(errno, buf, 1000) ) {
            fprintf(stderr, "strerror_r() failed.\n");
            abort();
        }
        if ( asprintf(errmsg, "tcgetattr() failed: %d", errno ) == -1 ) {
            fprintf(stderr, "Out of memory.\n");
            abort();
        }
        free( old );
        return NULL;
    }

    return old;
}

int restore_character_mode( struct termios* old, char** errmsg )
{
    if ( tcsetattr( STDIN_FILENO, TCSADRAIN, old ) == -1 ) {
        if ( asprintf(errmsg, "tcgetattr() failed: %d", errno ) == -1 ) {
            fprintf(stderr, "Out of memory.\n");
            abort();
        }
        return -1;
    }
    free( old );
    return 0;
}

