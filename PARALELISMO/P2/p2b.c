#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#include <time.h>

int MPI_FlattreeColectiva(const void *sendbuf, void *recvbuf, int count, 
                          MPI_Datatype datatype, MPI_Op op, int root, 
                          MPI_Comm comm) {
    int rank, numprocs, err;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &numprocs);

    if (op != MPI_SUM || datatype != MPI_INT) {
        return MPI_ERR_OP; // Solo soportamos suma y enteros
    }

    if (root < 0 || root >= numprocs) {
        return MPI_ERR_ROOT; // Verificamos que el root sea válido
    }

    int local_value = *((int*)sendbuf);
    int total = local_value;

    if (rank == root) {
        for (int i = 0; i < numprocs; i++) {
            if (i != root) { // El root no se envía a sí mismo
                int valor_recepcion;
                err = MPI_Recv(&valor_recepcion, count, datatype, i, 0, comm, MPI_STATUS_IGNORE);
                if (err != MPI_SUCCESS) {
                    return err; // Error en MPI_Recv
                }
                total += valor_recepcion;
            }
        }
        *((int*)recvbuf) = total;
    } else {
        err = MPI_Send(&local_value, count, datatype, root, 0, comm);
        if (err != MPI_SUCCESS) {
            return err; // Error en MPI_Send
        }
    }

    return MPI_SUCCESS;
}

int MPI_BinomialColectiva(void *buffer, int count, MPI_Datatype datatype, 
                          int root, MPI_Comm comm) {
    int rank, numprocs, err;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &numprocs);

    if (datatype != MPI_INT || root != 0) {
        return MPI_ERR_OP; // Solo soportamos enteros y root = 0
    }

    for (int pos = 1; pos < numprocs; pos *= 2) {
        if (rank < pos) {
            int dest = rank + pos;
            if (dest < numprocs) {
                err = MPI_Send(buffer, count, datatype, dest, 0, comm);
                if (err != MPI_SUCCESS) {
                    return err; // Error en MPI_Send
                }
            }
        } else if (rank < 2 * pos) {
            int source = rank - pos;
            err = MPI_Recv(buffer, count, datatype, source, 0, comm, MPI_STATUS_IGNORE);
            if (err != MPI_SUCCESS) {
                return err; // Error en MPI_Recv
            }
        }
    }

    return MPI_SUCCESS;
}

int main(int argc, char *argv[]) {
    int rank, numprocs, err;
    int n, count = 0, total_count = 0;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    srand(time(NULL) + rank);

    while (1) {
        if (rank == 0) {
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d", &n);
        }

        err = MPI_BinomialColectiva(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
        if (err != MPI_SUCCESS) {
            printf("Error en MPI_BinomialColectiva: %d\n", err);
            MPI_Abort(MPI_COMM_WORLD, err);
        }

        if (n == 0) break;

        count = 0;
        for (int i = rank; i < n; i += numprocs) {
            x = ((double) rand()) / RAND_MAX;
            y = ((double) rand()) / RAND_MAX;
            z = sqrt(x * x + y * y);
            if (z <= 1.0) count++;
        }

        err = MPI_FlattreeColectiva(&count, &total_count, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
        if (err != MPI_SUCCESS) {
            printf("Error en MPI_FlattreeColectiva: %d\n", err);
            MPI_Abort(MPI_COMM_WORLD, err);
        }

        if (rank == 0) {
            pi = ((double) total_count / n) * 4.0;
            printf("pi es aproximadamente %.16f, Error es %.16f\n", pi, fabs(pi - PI25DT));
        }
    }

    MPI_Finalize();
    return 0;
}



