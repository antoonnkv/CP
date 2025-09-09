#include "pthread.h"
#include "sem.h"

#include <stdio.h>


int sem_init(sem_t *s, int value) {

    if (value < 0) {
        perror("Value debe ser positivo");
        return 0;
    }
    pthread_mutex_init(&s->mutexSemaforo, NULL);
    s->valor = value;
    pthread_cond_init(&s->BARBER_BUSY, NULL);

    return 0;
}

int sem_destroy(sem_t *s) {

    pthread_mutex_destroy(&s->mutexSemaforo);
    pthread_cond_destroy(&s->BARBER_BUSY);

    return 0;
}

int sem_p(sem_t *s) {

    pthread_mutex_lock(&s->mutexSemaforo);
    while(s->valor <= 0) {
        pthread_cond_wait(&s->BARBER_BUSY, &s->mutexSemaforo); // esperar a que el haya un hueco
    }
    s->valor--;
    pthread_mutex_unlock(&s->mutexSemaforo);

    return 0;
}

int sem_v(sem_t *s) {
    pthread_mutex_lock(&s->mutexSemaforo);
    s->valor++;
    pthread_cond_signal(&s->BARBER_BUSY);
    pthread_mutex_unlock(&s->mutexSemaforo);
    return 0;
}

int sem_tryp(sem_t *s) { // 0 on sucess, -1 if already locked

    if (pthread_mutex_trylock(&s->mutexSemaforo) != 0) {
        return -1;
    }

    if (s->valor > 0) {
        s->valor--;
        pthread_mutex_unlock(&s->mutexSemaforo);
        return 0; 
    }
    pthread_mutex_unlock(&s->mutexSemaforo);
    return -1;
}
