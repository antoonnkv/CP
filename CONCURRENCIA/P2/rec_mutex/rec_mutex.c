#include "rec_mutex.h"
#include "pthread.h"
#include "stdio.h"


int rec_mutex_init(rec_mutex_t *m) {
    int aux =pthread_mutex_init(&m->mutex, NULL);
    if (aux == 0){
        m->propietario = 0;
        m->bloqueos = 0;
    }
    return aux;
}

int rec_mutex_destroy(rec_mutex_t *m) {
    return pthread_mutex_destroy(&m->mutex);
}

int rec_mutex_lock(rec_mutex_t *m) {
  pthread_t propietario = pthread_self();
  if(pthread_equal(m->propietario,propietario)) {
      m->bloqueos ++;
      return 0;
  }
  int aux = pthread_mutex_lock(&m->mutex);
  if(aux == 0) {
     m->propietario = propietario;
     m->bloqueos = 1;
  }
  return aux;
};

int rec_mutex_unlock(rec_mutex_t *m) {
    pthread_t propietario = pthread_self();
    if(!pthread_equal(m->propietario,propietario)) {
      return -1;
    }

    m->bloqueos --;

    if(m->bloqueos == 0) {
      m->propietario = 0;
      return pthread_mutex_unlock(&m->mutex);
    }
    return 0;
};

int rec_mutex_trylock(rec_mutex_t *m) {
    pthread_t propietario = pthread_self();
    if(pthread_equal(m->propietario,propietario)) {
      	m->bloqueos ++;
      	return 0;
    }else{
    	if(pthread_mutex_trylock(&m->mutex) == 0){
          	m->propietario = propietario;
          	m->bloqueos = 1;
            return 0;
        }else{
          	return -1;
        }
    }
}
