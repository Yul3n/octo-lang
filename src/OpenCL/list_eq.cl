kernel void
list_eq_kernel(global const int* A, global const int* B, global int* C)
{
    int id = get_global_id(0);
    C += (A[id] == B[id]) ? 0 : 1;
}
