#ifndef OPTIONS_H
#define OPTIONS_H

struct options {
    int num_threads;
    int buffer_size;
    int iterations;
    int delay;
    int print_wait;
};

int read_options(int argc, char **argv, struct options *opt);


#endif //OPTIONS_H
