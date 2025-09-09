#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"
#include "rec_mutex.h"

struct buffer {
    int *data;
    int size;
    rec_mutex_t *mutex; // Mutex para proteger el acceso al buffer
};

struct thread_info {
    pthread_t       thread_id;        // ID retornado por pthread_create()
    int             thread_num;       // Número del hilo (definido por la aplicación)
};

struct args {
    int             thread_num;       // Número del hilo
    int             delay;            // Retraso entre operaciones
    int             iterations;       // Cantidad de iteraciones
    struct buffer   *buffer;          // Buffer compartido
};


void *swap(void *ptr)
{
    struct args *args = ptr;

    while(args->iterations--) {
        int i, j, tmp;

        i = rand() % args->buffer->size;
        j = rand() % args->buffer->size;


        if (i < j)
        {
            rec_mutex_lock(&args->buffer->mutex[i]);
            rec_mutex_lock(&args->buffer->mutex[j]);
        }
        else
        {
            rec_mutex_lock(&args->buffer->mutex[j]);
            rec_mutex_lock(&args->buffer->mutex[i]);
        }

        printf("Thread %d swapping positions %d (== %d) and %d (== %d)\n",
            args->thread_num, i, args->buffer->data[i], j, args->buffer->data[j]);

        tmp = args->buffer->data[i];
        if(args->delay) usleep(args->delay); // Forzar cambio de contexto

        args->buffer->data[i] = args->buffer->data[j];
        if(args->delay) usleep(args->delay);

        args->buffer->data[j] = tmp;
        if(args->delay) usleep(args->delay);

        inc_count();

        rec_mutex_unlock(&args->buffer->mutex[i]);
        rec_mutex_unlock(&args->buffer->mutex[j]);
    }
    return NULL;
}



int cmp(const void *e1, const void *e2) {
    return (*(int *)e1 - *(int *)e2);
}

void print_buffer(struct buffer buffer) {
    for (int i = 0; i < buffer.size; i++)
        printf("%i ", buffer.data[i]);
    printf("\n");
}


void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct args *args;
    struct buffer buffer;

    srand(time(NULL));

    // Asignar memoria al buffer
    if ((buffer.data = malloc(opt.buffer_size * sizeof(int))) == NULL) {
        printf("Out of memory\n");
        exit(1);
    }
    buffer.size = opt.buffer_size;
    buffer.mutex = malloc(sizeof(rec_mutex_t) * buffer.size);


    for(int y =0; y < buffer.size ; y++) {
        rec_mutex_init(&buffer.mutex[y]);
    }



    for (i = 0; i < buffer.size; i++)
        buffer.data[i] = i;



    printf("Creating %d threads\n", opt.num_threads);

    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    args = malloc(sizeof(struct args) * opt.num_threads);

    if (threads == NULL || args == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }

    printf("Buffer before: ");
    print_buffer(buffer);

    // Crear hilos que ejecutan swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].thread_num = i;
        args[i].thread_num = i;
        args[i].buffer = &buffer;
        args[i].delay = opt.delay;
        args[i].iterations = opt.iterations;

        if (pthread_create(&threads[i].thread_id, NULL, swap, &args[i]) != 0) {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }

    }


    // Esperar que los hilos terminen
    for (i = 0; i <= opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);

    // Ordenar y mostrar el buffer después de la ejecución
    printf("Buffer after:  ");
    qsort(buffer.data, opt.buffer_size, sizeof(int), cmp);
    print_buffer(buffer);

    printf("Iterations: %d\n", get_count());

    // Liberar memoria y destruir mutex
    free(args);
    free(threads);
    free(buffer.data);


    for(int w = 0; w < buffer.size; w++){
        rec_mutex_destroy(&buffer.mutex[w]);
      }

    free(buffer.mutex);


    pthread_exit(NULL);
}

int main(int argc, char **argv)
{
    struct options opt;

    // Valores por defecto para las opciones
    opt.num_threads = 10;
    opt.buffer_size = 10;
    opt.iterations  = 10;
    opt.delay       = 10;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit(0);
}

