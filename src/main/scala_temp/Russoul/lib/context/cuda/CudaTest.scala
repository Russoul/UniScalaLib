package Russoul.lib.context.cuda

import jcuda.driver._
import jcuda.jcurand.JCurand._
import jcuda.jcurand.{JCurand, curandGenerator, curandRngType}
import jcuda.runtime.JCuda._
import jcuda.runtime.{JCuda, cudaDeviceProp, cudaMemcpyKind}
import jcuda.{Pointer, Sizeof}
/**
  * Created by Russoul on 08.08.2016.
  */





object CudaTest
{
  def test1() =
  {
    JCuda.setExceptionsEnabled(true)
    JCurand.setExceptionsEnabled(true)

    val n = 100

    val generator = new curandGenerator

    val hostData = new Array[Float](n)

    val deviceData = new Pointer()
    cudaMalloc(deviceData, n * Sizeof.FLOAT)
    curandCreateGenerator(generator, curandRngType.CURAND_RNG_PSEUDO_DEFAULT)
    curandSetPseudoRandomGeneratorSeed(generator, 1234)
    curandGenerateUniform(generator, deviceData, n)

    cudaMemcpy(Pointer.to(hostData), deviceData, n * Sizeof.FLOAT, cudaMemcpyKind.cudaMemcpyDeviceToHost)

    hostData.foreach(println)

    curandDestroyGenerator(generator)
    cudaFree(deviceData)
  }

  def test2() =
  {
    val count = new Array[Int](1)
    cudaGetDeviceCount(count)
    for(i <- 0 until count(0)){
      val props:cudaDeviceProp = new cudaDeviceProp
      cudaGetDeviceProperties(props, i)

      println("Got device: "+props.getName)
      println("Max threads per block: "+props.maxThreadsPerBlock)
    }


    JCudaDriver.cuInit(0)
    val device = new CUdevice()
    JCudaDriver.cuDeviceGet(device, 0)
    val context = new CUcontext()
    JCudaDriver.cuCtxCreate(context, 0, device)

    val module = new CUmodule()
    JCudaDriver.cuModuleLoad(module, "assets/kernel/add.ptx")

    // Obtain a function pointer to the kernel function.
    val function = new CUfunction()
    JCudaDriver.cuModuleGetFunction(function, module, "add")


    val numElements = 100000

    // Allocate and fill the host input data
    val hostInputA = new Array[Float](numElements)
    val hostInputB = new Array[Float](numElements)
    val hostOutputC = new Array[Float](numElements)
    for(i <- 0 until numElements)
    {
      hostInputA(i) = i
      hostInputB(i) = i
    }

    // Allocate the device input data, and copy the
    // host input data to the device
    val deviceInputA = new CUdeviceptr()
    JCudaDriver.cuMemAlloc(deviceInputA, numElements * Sizeof.FLOAT)
    JCudaDriver.cuMemcpyHtoD(deviceInputA, Pointer.to(hostInputA),
      numElements * Sizeof.FLOAT)
    val deviceInputB = new CUdeviceptr()
    JCudaDriver.cuMemAlloc(deviceInputB, numElements * Sizeof.FLOAT)
    JCudaDriver.cuMemcpyHtoD(deviceInputB, Pointer.to(hostInputB),
      numElements * Sizeof.FLOAT)

    // Allocate device output memory
    val deviceOutput = new CUdeviceptr()
    JCudaDriver.cuMemAlloc(deviceOutput, numElements * Sizeof.FLOAT)

    // Set up the kernel parameters: A pointer to an array
    // of pointers which point to the actual values.
    val kernelParameters = Pointer.to(
      Pointer.to(Array(numElements)),
      Pointer.to(deviceInputA),
      Pointer.to(deviceInputB),
      Pointer.to(deviceOutput)
    )


    val numThreads = 128
    val numBlocks = (numElements + 127)/128
    // Call the kernel function.
    JCudaDriver.cuLaunchKernel(function,
      numBlocks,  1, 1,      // blocks
      numThreads, 1, 1,      // threads
      0, null,               // Shared memory size and stream
      kernelParameters, null // Kernel- and extra parameters
    )

    JCudaDriver.cuCtxSynchronize()

    JCudaDriver.cuMemcpyDtoH(Pointer.to(hostOutputC), deviceOutput, 4 * numElements)

    hostOutputC.foreach(println)


    JCudaDriver.cuMemFree(deviceInputA)
    JCudaDriver.cuMemFree(deviceInputB)
    JCudaDriver.cuMemFree(deviceOutput)
  }

  test2()

}
