#include "rw_mutex.h"
#include "stdio.h"
#include <pthread.h>

int rw_mutex_init(rw_mutex_t *m) {

    if(pthread_mutex_init(&m->mutex, NULL)) return -1;
    m->lectores = 0;
    m->escritores = 0;
    m->escritores_espera = 0;
    if(pthread_cond_init(&m->hay_escritores, NULL)) return -1;
    if(pthread_cond_init(&m->hay_alguien, NULL)) return -1;

    return 0;
}

int rw_mutex_destroy(rw_mutex_t *m) {
    pthread_mutex_destroy(&m->mutex);
    pthread_cond_destroy(&m->hay_escritores);
    pthread_cond_destroy(&m->hay_alguien);
    return 0;
}

int rw_mutex_readlock(rw_mutex_t *m) {
    pthread_mutex_lock(&m->mutex);

    while(m->escritores > 0 || m->escritores_espera > 0)
    	pthread_cond_wait(&m->hay_escritores, &m->mutex);

    m->lectores++;

    pthread_mutex_unlock(&m->mutex);

    return 0;
}

int rw_mutex_writelock(rw_mutex_t *m) {
    pthread_mutex_lock(&m->mutex);

    while(m->lectores > 0 || m->escritores > 0){
     m->escritores_espera++;
     pthread_cond_wait(&m->hay_alguien, &m->mutex);
    }

    m->escritores = 1;

    pthread_mutex_unlock(&m->mutex);
    return 0;
}

int rw_mutex_readunlock(rw_mutex_t *m) {
    pthread_mutex_lock(&m->mutex);

    m->lectores--;

    if(m->lectores == 0 && m->escritores_espera > 0){
      pthread_cond_signal(&m->hay_alguien);
      m->escritores_espera--;
    }


    pthread_mutex_unlock(&m->mutex);
    return 0;
}

int rw_mutex_writeunlock(rw_mutex_t *m) {
    pthread_mutex_lock(&m->mutex);

    m->escritores = 0;

    if(m->escritores_espera > 0){
      m->escritores_espera--;
      pthread_cond_signal(&m->hay_alguien);
    }

    pthread_cond_broadcast(&m->hay_escritores);

    pthread_mutex_unlock(&m->mutex);
    return 0;
}
