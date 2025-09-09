#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "options.h"
#include "sem.h"
#include "stdbool.h"

#define MAX_SILLAS 100

struct semaforos {
    sem_t semaforoBarberos;
    sem_t semaforoClientes;
    sem_t semSillasLibres;
    sem_t semNoQuedanClientes;
    int sillasLibres;
    bool noQuedanClientes;
};

struct clientes {
    int id;
    struct semaforos *semaforos;
};

struct barberos {
    int id;
    int tiempoDeCorte;
    struct semaforos *semaforos;
};

void *funcionBarberos(void *args) {
    struct barberos *barberos = (struct barberos *) args;

    int cortes = 0;

    while (1) {
        
        sem_p(&barberos->semaforos->semNoQuedanClientes);
        if (barberos->semaforos->noQuedanClientes) {
            sem_v(&barberos->semaforos->semNoQuedanClientes);
            break;  // ya no hay clientes
        }
        sem_v(&barberos->semaforos->semNoQuedanClientes);

        sem_p(&barberos->semaforos->semaforoClientes); // esperar un cliente


        sem_p(&barberos->semaforos->semSillasLibres);
        barberos->semaforos->sillasLibres++;
        cortes++;

        printf("Barbero %d atendiendo a un cliente, lleva %d\n", barberos->id, cortes);

        usleep(barberos->tiempoDeCorte);

        sem_v(&barberos->semaforos->semSillasLibres);
        sem_v(&barberos->semaforos->semaforoBarberos); // decirle al cliente que ha terminado
    }
    return NULL;
}

void *funcionClientes(void *args) {
    struct clientes *cliente = (struct clientes *) args;
    int aux; 
    
    aux = sem_tryp(&cliente->semaforos->semaforoBarberos);

    // Como no hay orden no es necesario mirar sillasLibres == MAX_SILLAS

    if(aux == 0){
        printf("Cliente %d va directamente con el barbero\n", cliente->id);
        sem_v(&cliente->semaforos->semaforoClientes);
        return NULL;
    }

    sem_p(&cliente->semaforos->semSillasLibres);

    if (cliente->semaforos->sillasLibres > 0) {
        cliente->semaforos->sillasLibres--;
        printf("Cliente %d toma una silla, ahora quedan %d sillas libres\n", cliente->id, cliente->semaforos->sillasLibres);
        sem_v(&cliente->semaforos->semSillasLibres);
        sem_v(&cliente->semaforos->semaforoClientes);

        sem_p(&cliente->semaforos->semaforoBarberos); // esperar al barbero
    } else {
        printf("Cliente %d se va porque no hay sillas disponibles.\n", cliente->id);
        sem_v(&cliente->semaforos->semSillasLibres);
    }
    return NULL;
}

void start_threads(struct options opt) {

    pthread_t *clientes = malloc(sizeof(pthread_t) * opt.customers);
    pthread_t *barberos = malloc(sizeof(pthread_t) * opt.barbers);
    struct semaforos *semaforos = malloc(sizeof(struct semaforos));
    struct barberos *structBarberos = malloc(sizeof(struct barberos) * opt.barbers);
    struct clientes *structClientes = malloc(sizeof(struct clientes) * opt.customers);



    if (!clientes || !barberos || !semaforos || !structClientes || !structBarberos) {
        printf("No hay suficiente memoria\n");
        exit(1);
    }

    sem_init(&semaforos->semaforoBarberos, opt.barbers);
    sem_init(&semaforos->semaforoClientes, 0);
    sem_init(&semaforos->semSillasLibres, MAX_SILLAS);
    sem_init(&semaforos->semNoQuedanClientes, 1);  // semaforo init(sem,0,1)= actua como unmutex

    semaforos->noQuedanClientes = false;
    semaforos->sillasLibres = MAX_SILLAS;

    for (int i = 0; i < opt.customers; i++) {
        structClientes[i].id = i;
        structClientes[i].semaforos = semaforos;

        if (pthread_create(&clientes[i], NULL, funcionClientes, &structClientes[i]) != 0) {
            printf("No se puede crear el thread %d de los clientes\n", i);
            exit(1);
        }
    }

    for (int i = 0; i < opt.barbers; i++) {
        structBarberos[i].id = i;
        structBarberos[i].semaforos = semaforos;
        structBarberos[i].tiempoDeCorte = opt.cut_time;

        if (pthread_create(&barberos[i], NULL, funcionBarberos, &structBarberos[i]) != 0) {
            printf("No se puede crear el thread %d de los barberos\n", i);
            exit(1);
        }
    }

    for (int i = 0; i < opt.customers; i++) {
        pthread_join(clientes[i], NULL);
    }

    sem_p(&semaforos->semNoQuedanClientes);
    semaforos->noQuedanClientes = true;
    sem_v(&semaforos->semNoQuedanClientes);

    
    //  En caso de que hayan barberos atrapados en el bucle rescatarlos
    for (int i = 0; i < opt.barbers; i++) {
        sem_v(&semaforos->semaforoClientes);
    }

    for (int i = 0; i < opt.barbers; i++) {
        pthread_join(barberos[i], NULL);
    }

    sem_destroy(&semaforos->semaforoBarberos);
    sem_destroy(&semaforos->semaforoClientes);
    sem_destroy(&semaforos->semSillasLibres);
    sem_destroy(&semaforos->semNoQuedanClientes);



    free(clientes);
    free(barberos);
    free(semaforos);
    free(structClientes);
    free(structBarberos);
}

int main(int argc, char **argv) {
    struct options opt;

    // Default values for the options
    opt.barbers = 2;
    opt.customers = 300;
    opt.cut_time  = 30000;


    read_options(argc, argv, &opt);
    start_threads(opt);
    exit(0);
}
