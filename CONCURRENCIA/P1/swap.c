#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "op_count.h"
#include "options.h"
#include <stdbool.h>

struct buffer {
    int *data;
    int size;
    pthread_mutex_t *mutex; // Mutex para proteger el acceso al buffer
};

struct thread_info {
    pthread_t       thread_id;        // ID retornado por pthread_create()
    int             thread_num;       // Número del hilo (definido por la aplicación)
};

struct args {
    int             thread_num;         // Número del hilo
    int             delay;              // Retraso entre operaciones
    int             *iterations;        // Numero iteraciones totales compartidas resultantes
    pthread_mutex_t *mutexIter;         // Mutex para proteger las iteraciones totales
    struct buffer   *buffer;            // Buffer compartido
};


struct args_Print {
    int print_time;
    struct buffer *buffer;
    pthread_mutex_t *mutex;
    bool *end;
};

struct print_Thread {
    pthread_t thread_id;
};

void *swap(void *ptr)
{
    struct args *args = ptr;
    int i, j, tmp, iterationsDone = 0;

    while(1){

        pthread_mutex_lock(args->mutexIter);
            if(*(args->iterations) <= 0){
                pthread_mutex_unlock(args->mutexIter);
                break;
            }
            (*(args->iterations))--;


            pthread_mutex_unlock(args->mutexIter);


            do {
                i = rand() % args->buffer->size;
                j = rand() % args->buffer->size;
            } while(i==j);

            if (i < j)
            {
                pthread_mutex_lock(&args->buffer->mutex[i]);
                pthread_mutex_lock(&args->buffer->mutex[j]);
            }
            else 
            {
                pthread_mutex_lock(&args->buffer->mutex[j]);
                pthread_mutex_lock(&args->buffer->mutex[i]);
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

            iterationsDone++;

            pthread_mutex_unlock(&args->buffer->mutex[i]);
            pthread_mutex_unlock(&args->buffer->mutex[j]);
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


void *print_per(void *ptr) {

    struct args_Print *args = ptr;

    while(1) 
    {
        pthread_mutex_lock(args->mutex);
        if(*args->end) {
            pthread_mutex_unlock(args->mutex);
            return NULL;
        }
        pthread_mutex_unlock(args->mutex);
        
        for (int i = 0; i < args->buffer->size; i++)
            pthread_mutex_lock(&args->buffer->mutex[i]);
        
        print_buffer(*args->buffer);

        for (int i = 0; i < args->buffer->size; i++)
            pthread_mutex_unlock(&args->buffer->mutex[i]);

        usleep(args->print_time);
    }

    return NULL;
}


void start_threads(struct options opt)
{
    int i;
    struct thread_info *threads;
    struct print_Thread *printThread;
    struct args *args;
    struct buffer buffer;
    struct args_Print *printArgs;
    bool *end;
    pthread_mutex_t *mutexP;        // mutex printArray
    pthread_mutex_t *mutexI;        // mutex iteraciones


    srand(time(NULL));

    // Asignar memoria al buffer
    if ((buffer.data = malloc(opt.buffer_size * sizeof(int))) == NULL) {
        printf("Out of memory\n");
        exit(1);
    }

    buffer.size = opt.buffer_size;
    buffer.mutex = malloc(sizeof(pthread_mutex_t) * buffer.size);

    for(int y =0; y < buffer.size ; y++)
        pthread_mutex_init(&buffer.mutex[y], NULL);

    for (i = 0; i < buffer.size; i++)
        buffer.data[i] = i;

    printf("Creating %d threads\n", opt.num_threads);

    threads = malloc(sizeof(struct thread_info) * opt.num_threads);
    printThread = malloc(sizeof(struct print_Thread));
    args = malloc(sizeof(struct args) * opt.num_threads);
    printArgs = malloc(sizeof(struct args_Print));
    end = malloc(sizeof(bool));
    mutexP = malloc(sizeof(pthread_mutex_t));
    mutexI = malloc(sizeof(pthread_mutex_t));

    if (!threads || !printThread || !args || !printArgs || !end || 
        !mutexP || !buffer.mutex || !mutexI) {
        printf("Not enough memory\n");
        exit(1);
    }


    pthread_mutex_init(mutexP, NULL);

    printArgs->mutex = mutexP;

    pthread_mutex_init(mutexI, NULL);

    printf("Buffer before: ");
    print_buffer(buffer);

    // Crear hilos que ejecutan swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].thread_num = i;
        args[i].thread_num = i;
        args[i].buffer = &buffer;
        args[i].delay = opt.delay;
        args[i].iterations = &opt.iterations; 
        args[i].mutexIter = mutexI;


        if (pthread_create(&threads[i].thread_id, NULL, swap, &args[i]) != 0) {
            printf("Could not create thread #%d\n", i);
            exit(1);
        }
    }

    printArgs->print_time = opt.print_wait;
    printArgs->buffer = &buffer;
    *end = false;
    printArgs->end = end;

     
    if (pthread_create(&printThread->thread_id, NULL, print_per, printArgs) != 0) {
        printf("Could not create thread #%d\n", i);
        exit(1);
    }

    // Esperar que los hilos terminen
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].thread_id, NULL);

    pthread_mutex_lock(mutexP);
    *end = true;
    pthread_mutex_unlock(mutexP);

    pthread_join(printThread->thread_id, NULL);

    // Ordenar y mostrar el buffer después de la ejecución
    printf("\nBuffer after:  ");
    qsort(buffer.data, opt.buffer_size, sizeof(int), cmp);
    print_buffer(buffer);

    printf("Iterations: %d\n", get_count());
    for(int w = 0; w < buffer.size; w++){
        pthread_mutex_destroy(&buffer.mutex[w]);
    }

    pthread_mutex_destroy(mutexP);
    pthread_mutex_destroy(mutexI);

    // Liberar memoria y destruir mutex
    free(mutexI);
    free(args);
    free(threads);
    free(buffer.data);
    free(buffer.mutex);
    free(printThread);
    free(end);
    free(mutexP);
    free(printArgs);
    
    pthread_exit(NULL);
}

int main(int argc, char **argv)
{
    struct options opt;

    // Valores por defecto para las opciones
    opt.num_threads = 10;
    opt.buffer_size = 10;
    opt.iterations  = 100;
    opt.delay       = 10;
    opt.print_wait  = 200;

    read_options(argc, argv, &opt);

    start_threads(opt);

    exit(0);
}