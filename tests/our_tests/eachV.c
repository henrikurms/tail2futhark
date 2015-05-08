#include <OpenCL/opencl.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <sys/time.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
static double futhark_main();
float futhark_toFloat32(int x)
{
    return x;
}
int futhark_trunc32(float x)
{
    return x;
}
float futhark_log32(float x)
{
    return log(x);
}
float futhark_sqrt32(float x)
{
    return sqrt(x);
}
float futhark_exp32(float x)
{
    return exp(x);
}
double futhark_toFloat64(int x)
{
    return x;
}
int futhark_trunc64(double x)
{
    return x;
}
double futhark_log64(double x)
{
    return log(x);
}
double futhark_sqrt64(double x)
{
    return sqrt(x);
}
double futhark_exp64(double x)
{
    return exp(x);
}
int timeval_subtract(struct timeval* result, struct timeval* t2,
                     struct timeval* t1)
{
    unsigned int resolution = 1000000;
    long diff = t2->tv_usec + resolution * t2->tv_sec - (t1->tv_usec +
                                                         resolution *
                                                         t1->tv_sec);
    
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return diff < 0;
}
struct array_reader {
    char* elems;
    int64_t n_elems_space;
    int64_t elem_size;
    int64_t n_elems_used;
    int64_t* shape;
    int(* elem_reader)(void*);
};
int peekc()

{
    int c = getchar();
    
    ungetc(c, stdin);
    return c;
}
void skipspaces()

{
    int c = getchar();
    
    if (isspace(c)) {
        skipspaces();
    } else if (c == '/') {
        // Skip to end of line.
        for (; c != '\n' && c != EOF; c = getchar())
            ;
        // Next line may have more spaces.
        skipspaces();
    } else if (c != EOF) {
        ungetc(c, stdin);
    }
}
int read_elem(struct array_reader* reader)
{
    int ret;
    
    if (reader->n_elems_used == reader->n_elems_space) {
        reader->n_elems_space *= 2;
        reader->elems = realloc(reader->elems, reader->n_elems_space *
                                reader->elem_size);
    }
    ret = reader->elem_reader(reader->elems + reader->n_elems_used *
        reader->elem_size);
    if (ret == 0) {
        reader->n_elems_used++;
    }
    return ret;
}
int read_array_elems(struct array_reader* reader, int dims)
{
    int c;
    int ret;
    int first = 1;
    char* knows_dimsize = calloc(dims, sizeof(char));
    int cur_dim = dims - 1;
    int64_t* elems_read_in_dim = calloc(dims, sizeof(int64_t));
    
    while (1) {
        skipspaces();
        c = getchar();
        if (c == ']') {
            if (knows_dimsize[cur_dim]) {
                if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
                    ret = 1;
                    break;
                }
            } else {
                knows_dimsize[cur_dim] = 1;
                reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
            }
            if (cur_dim == 0) {
                ret = 0;
                break;
            } else {
                cur_dim--;
                elems_read_in_dim[cur_dim]++;
            }
        } else if (c == ',') {
            skipspaces();
            c = getchar();
            if (c == '[') {
                if (cur_dim == dims - 1) {
                    ret = 1;
                    break;
                }
                first = 1;
                cur_dim++;
                elems_read_in_dim[cur_dim] = 0;
            } else if (cur_dim == dims - 1) {
                ungetc(c, stdin);
                ret = read_elem(reader);
                if (ret != 0) {
                    break;
                }
                elems_read_in_dim[cur_dim]++;
            } else {
                ret = 1;
                break;
            }
        } else if (c == EOF) {
            ret = 1;
            break;
        } else if (first) {
            if (c == '[') {
                if (cur_dim == dims - 1) {
                    ret = 1;
                    break;
                }
                cur_dim++;
                elems_read_in_dim[cur_dim] = 0;
            } else {
                ungetc(c, stdin);
                ret = read_elem(reader);
                if (ret != 0) {
                    break;
                }
                elems_read_in_dim[cur_dim]++;
                first = 0;
            }
        } else {
            ret = 1;
            break;
        }
    }
    free(knows_dimsize);
    free(elems_read_in_dim);
    return ret;
}
int read_array(int64_t elem_size, int(* elem_reader)(void*), void** data,
               int64_t* shape, int64_t dims)
{
    int ret;
    struct array_reader reader;
    int64_t read_dims = 0;
    
    while (1) {
        int c;
        
        skipspaces();
        c = getchar();
        if (c == '[') {
            read_dims++;
        } else {
            if (c != EOF) {
                ungetc(c, stdin);
            }
            break;
        }
    }
    if (read_dims != dims) {
        return 1;
    }
    reader.shape = shape;
    reader.n_elems_used = 0;
    reader.elem_size = elem_size;
    reader.n_elems_space = 16;
    reader.elems = calloc(elem_size, reader.n_elems_space);
    reader.elem_reader = elem_reader;
    ret = read_array_elems(&reader, dims);
    *data = reader.elems;
    return ret;
}
int read_int(void* dest)
{
    skipspaces();
    if (scanf("%d", (int*) dest) == 1) {
        return 0;
    } else {
        return 1;
    }
}
int read_char(void* dest)
{
    skipspaces();
    if (scanf("%c", (char*) dest) == 1) {
        return 0;
    } else {
        return 1;
    }
}
int read_double(void* dest)
{
    skipspaces();
    if (scanf("%lf", (double*) dest) == 1) {
        return 0;
    } else {
        return 1;
    }
}
int read_float(void* dest)
{
    skipspaces();
    if (scanf("%f", (float*) dest) == 1) {
        return 0;
    } else {
        return 1;
    }
}
cl_context fut_cl_context;
cl_command_queue fut_cl_queue;
cl_build_status build_opencl_kernel(cl_program program, cl_device_id device,
                                    const char* options)
{
    cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);
    
    // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
    if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
        assert(ret_val == 0);
    }
    
    cl_build_status build_status;
    
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_STATUS,
                                    sizeof(cl_build_status), &build_status,
                                    NULL);
    assert(ret_val == 0);
    if (build_status != CL_SUCCESS) {
        char* build_log;
        size_t ret_val_size;
        
        ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG,
                                        0, NULL, &ret_val_size);
        assert(ret_val == 0);
        build_log = malloc(ret_val_size + 1);
        clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG,
                              ret_val_size, build_log, NULL);
        assert(ret_val == 0);
        // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
        build_log[ret_val_size] = '\0';
        fprintf(stderr, "Build log:\n%s", build_log);
        free(build_log);
    }
    return build_status;
}
void setup_opencl()

{
    cl_int error;
    cl_platform_id platform;
    cl_device_id device;
    cl_uint platforms,  devices;
    
    // Fetch the Platform and Device IDs; we only want one.
    error = clGetPlatformIDs(1, &platform, &platforms);
    assert(error == 0);
    assert(platforms > 0);
    error = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, &devices);
    assert(error == 0);
    assert(devices > 0);
    
    cl_context_properties properties[] = {CL_CONTEXT_PLATFORM,
                                          (cl_context_properties) platform, 0};
    
    // Note that nVidia's OpenCL requires the platform property
    fut_cl_context = clCreateContext(properties, 1, &device, NULL, NULL,
                                     &error);
    fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);
}
static double futhark_main()
{
    double scalar_out_2;
    unsigned char* mem_3;
    
    mem_3 = malloc(32);
    *(int*) &mem_3[0 * 1 * 4 + 0] = 1;
    *(int*) &mem_3[1 * 1 * 4 + 0] = 2;
    *(int*) &mem_3[2 * 1 * 4 + 0] = 3;
    *(int*) &mem_3[3 * 1 * 4 + 0] = 4;
    *(int*) &mem_3[4 * 1 * 4 + 0] = 5;
    *(int*) &mem_3[5 * 1 * 4 + 0] = 6;
    *(int*) &mem_3[6 * 1 * 4 + 0] = 7;
    *(int*) &mem_3[7 * 1 * 4 + 0] = 8;
    
    int arg_25;
    int acc_1;
    
    acc_1 = 0;
    for (int i_0 = 0; i_0 < 8; i_0++) {
        int t_x_22;
        
        t_x_22 = *(int*) &mem_3[i_0 * 1 * 4 + 0];
        
        int res_23;
        
        res_23 = t_x_22 + 2;
        
        int res_24;
        
        res_24 = acc_1 + res_23;
        
        int acc_tmp_4;
        
        acc_tmp_4 = res_24;
        acc_1 = acc_tmp_4;
    }
    arg_25 = acc_1;
    
    double res_27;
    
    res_27 = futhark_toFloat64(arg_25);
    scalar_out_2 = res_27;
    
    double retval_0;
    
    retval_0 = scalar_out_2;
    return retval_0;
}
int main(int argc, char** argv)
{
    struct timeval t_start,  t_end,  t_diff;
    unsigned long elapsed_usec;
    
    setup_opencl();
    {
        double scalar_out_2;
        double main_ret_1;
        
        gettimeofday(&t_start, NULL);
        main_ret_1 = futhark_main();
        gettimeofday(&t_end, NULL);
        scalar_out_2 = main_ret_1;
        printf("%.6f", scalar_out_2);
        printf("\n");
    }
    ;
    if (argc == 3 && strcmp(argv[1], "-t") == 0) {
        FILE * runtime_file;
        runtime_file = fopen(argv[2], "w");
        if (runtime_file == NULL) {
            fprintf(stderr, "Cannot open %s: %s\n", argv[2], strerror(errno));
            exit(1);
        }
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed_usec = t_diff.tv_sec * 1e6 + t_diff.tv_usec;
        fprintf(runtime_file, "%ld\n", elapsed_usec / 1000);
        fclose(runtime_file);
    }
    return 0;
}
