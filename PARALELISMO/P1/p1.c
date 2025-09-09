#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#include <time.h>

int main(int argc, char *argv[]) {
    int rank, numprocs;
    int i, done = 0, n, count = 0, total_count;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    
    srand(time(NULL) + rank);
    
    while (!done) {
        if (rank == 0) {
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d", &n);
            
            for (i = 1; i < numprocs; i++) {
                MPI_Send(&n, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            }
        } else {
            MPI_Recv(&n, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        
        if (n == 0) {
            done = 1;
            break;
        }
        
        count = 0;
        
        for (i = rank + 1; i <= n; i += numprocs) {
            x = ((double) rand()) / ((double) RAND_MAX);
            y = ((double) rand()) / ((double) RAND_MAX);
            z = sqrt((x*x) + (y*y));
            
            if (z <= 1.0)
                count++;
        }
        
        if (rank == 0) {
            total_count = count;
            int temp_count;
            
            for (i = 1; i < numprocs; i++) {
                MPI_Recv(&temp_count, 1, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                total_count += temp_count;
            }
            
            pi = ((double) total_count / (double) n) * 4.0;
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
        } else {
            MPI_Send(&count, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
        }
    }
    
    MPI_Finalize();
    return 0;
}


