/*The Mandelbrot set is a fractal that is defined as the set of points c
in the complex plane for which the sequence z_{n+1} = z_n^2 + c
with z_0 = 0 does not tend to infinity.*/

/*This code computes an image of the Mandelbrot set.*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mpi/mpi.h>

#define DEBUG 1

#define          X_RESN  1024  /* x resolution */
#define          Y_RESN  1024  /* y resolution */

/* Boundaries of the mandelbrot set */
#define           X_MIN  -2.0
#define           X_MAX   2.0
#define           Y_MIN  -2.0
#define           Y_MAX   2.0

/* More iterations -> more detailed image & higher computational cost */
#define   maxIterations  1000

typedef struct complextype
{
  float real, imag;
} Compl;

static inline double get_seconds(struct timeval t_ini, struct timeval t_end)
{
  return (t_end.tv_usec - t_ini.tv_usec) / 1E6 +
         (t_end.tv_sec - t_ini.tv_sec);
}

int main (int argc, char *argv[] )
{
  /* Mandelbrot variables */
  int i, j, k,filas_extra,rank,numprocs,filas_repartidas2,filas_repartidas;
  Compl z, c;
  float lengthsq, temp;


  /* Timestamp variables */
  struct timeval tiCompu, tfCompu, tiComunic,tfComunic;
  float tiempoTot,tiempoMax,tiempoTotCompu,tiempoTotComunic;


  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

  filas_repartidas = Y_RESN / numprocs;

  if(Y_RESN % numprocs != 0) filas_repartidas++;

  filas_extra = numprocs * filas_repartidas - Y_RESN;

  int *vres, *res[Y_RESN+filas_extra];
  int *mloc, *loc[filas_repartidas];


  if(!rank){
 /* Allocate result matrix of Y_RESN x X_RESN */
      vres = (int *) malloc((Y_RESN+filas_extra) * X_RESN * sizeof(int));
      if (!vres)
      {
        fprintf(stderr, "Error allocating memory\n");
        return 1;
      }
      for (i = 0; i < (Y_RESN+filas_extra); i++)
      res[i] = vres + i * X_RESN;
  }

  mloc = (int *) malloc(filas_repartidas * X_RESN * sizeof(int));

  if (!mloc)
  {
    fprintf(stderr, "Error allocating memory\n");
    return 1;
  }

  for (i = 0; i < filas_repartidas; i++)  loc[i] = mloc + i * X_RESN;


  if(rank < numprocs -1) filas_repartidas2 = filas_repartidas;
  else filas_repartidas2 = Y_RESN - (numprocs -1) * filas_repartidas;  
 
  /* Start measuring time */
  gettimeofday(&tiCompu, NULL);

  /* Calculate and draw points */
  for (i = 0; i < filas_repartidas2; i++)
  {
    float imag = Y_MAX - (i + rank * filas_repartidas) * (Y_MAX - Y_MIN) / Y_RESN;
    for (j = 0; j < X_RESN; j++)
    {
      float real = X_MIN + j * (X_MAX - X_MIN) / X_RESN;

      z.real = z.imag = 0.0;
      c.real = real;
      c.imag = imag;
      k = 0;

      do
      {
        temp = z.real * z.real - z.imag * z.imag + c.real;
        z.imag = 2.0 * z.real * z.imag + c.imag;
        z.real = temp;
        lengthsq = z.real * z.real + z.imag * z.imag;
        k++;
      } while (lengthsq < 4.0 && k < maxIterations);

      loc[i][j] = (k >= maxIterations) ? 0 : k;

    }
  }

  /* End measuring time */
  gettimeofday(&tfCompu, NULL);


  gettimeofday(&tiComunic, NULL);

  MPI_Gather(mloc, filas_repartidas * X_RESN, MPI_INT,
    vres, filas_repartidas * X_RESN, MPI_INT,
    0, MPI_COMM_WORLD);


  gettimeofday(&tfComunic, NULL); 
  
  tiempoTotCompu = get_seconds(tiCompu, tfCompu);
  tiempoTotComunic = get_seconds(tiComunic,tfComunic);
  tiempoTot = tiempoTotCompu + tiempoTotComunic;


  fprintf(stderr,"Proceso %d -> Computación: %f s | Comunicación: %f s | Total: %f s\n",rank, tiempoTotCompu, tiempoTotComunic, tiempoTot);


 
  MPI_Reduce(&tiempoTot, &tiempoMax, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD);



  if(!rank){

    fprintf(stderr, "Tiempo máximo entre procesos: %f\n", tiempoMax);

    if (DEBUG)
    {
      for (i = 0; i < Y_RESN; i++)
      {
        for (j = 0; j < X_RESN; j++)
          printf("%3d ", res[i][j]);
        printf("\n");
      }
    }
  
    free(vres);
  }

  free(mloc);

  MPI_Finalize();

  return 0;
}
