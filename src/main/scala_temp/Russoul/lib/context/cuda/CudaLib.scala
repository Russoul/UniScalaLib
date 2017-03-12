package Russoul.lib.context.cuda

import java.io.File

import Russoul.lib.common.utils.{FileUtils, vector}
import jcuda.Pointer
import jcuda.driver._
import jcuda.jcurand.JCurand
import jcuda.runtime.JCuda._
import jcuda.runtime.{JCuda, cudaDeviceProp}

/**
  * Created by Russoul on 08.08.2016.
  */


class CudaProcess (protected val pathToPtxFiles:String)
{
  protected val process = CudaLib.newProcess()
  protected var module:CUmodule = null

  loadAndLinkPtxFiles(pathToPtxFiles)



  private def loadAndLinkPtxFiles(pathToPtxFiles:String): Unit =
  {
    val linkState = new CUlinkState()
    val jitOptions = new JITOptions()
    JCudaDriver.cuLinkCreate(jitOptions, linkState)

    val files = new vector[File]()
    FileUtils.listOfFilesInDir(new File(pathToPtxFiles), files)

    for(file <- files){
      val name = file.getAbsolutePath
      JCudaDriver.cuLinkAddFile(linkState, CUjitInputType.CU_JIT_INPUT_PTX, name, jitOptions)
    }

    val sz = new Array[Long](1)
    val image = new Pointer()
    JCudaDriver.cuLinkComplete(linkState, image, sz)

    module = new CUmodule()
    JCudaDriver.cuModuleLoadDataEx(module, image, 0, new Array[Int](0), Pointer.to(new Array[Int](0)))
    JCudaDriver.cuLinkDestroy(linkState)


  }


  def getFunction(name:String) =
  {
    val function = new CUfunction()
    JCudaDriver.cuModuleGetFunction(function, module, name)
    function
  }

  def launchOneDim(function:CUfunction, numBlocks:Int, numThreads:Int, kernelParameters:Pointer): Unit =
  {
    JCudaDriver.cuLaunchKernel(function,
      numBlocks,  1, 1,      // blocks
      numThreads, 1, 1,      // threads
      0, null,               // Shared memory size and stream
      kernelParameters, null )// Kernel- and extra parameters
  }

  def waitUntilDone(): Unit =
  {
    JCudaDriver.cuCtxSynchronize()
  }
}


object CudaLib
{

  def enableExceptions(enable:Boolean = true) =
  {
    JCuda.setExceptionsEnabled(enable)
    JCudaDriver.setExceptionsEnabled(enable)
    JCurand.setExceptionsEnabled(true)
  }

  def cumalloc(size:Int): CUdeviceptr =
  {
    val p = new CUdeviceptr
    JCudaDriver.cuMemAlloc(p, size)
    p
  }

  def cufree(p:CUdeviceptr) = JCudaDriver.cuMemFree(p)

  def cucpyHToD(dst:CUdeviceptr, src:Pointer, size:Int): Unit =
  {
    JCudaDriver.cuMemcpyHtoD(dst, src, size)
  }

  def cucpyDToH(dst:Pointer, src:CUdeviceptr, size:Int): Unit =
  {
    JCudaDriver.cuMemcpyDtoH(dst, src, size)
  }


  def newPointer(toNumber:Int) =
  {
    val array = Array[Int](toNumber)
    Pointer.to(array)
  }

  def newPointer(toNumber:Float) =
  {
    val array = Array(toNumber)
    Pointer.to(array)
  }

  def newPointer(toNumber:Long) =
  {
    val array = Array(toNumber)
    Pointer.to(array)
  }

  def getDeviceProperties(num:Int = 0) =
  {
    val props:cudaDeviceProp = new cudaDeviceProp
    cudaGetDeviceProperties(props, num)

    props
  }

  def newProcess() =
  {
    JCudaDriver.cuInit(0)
    val device = new CUdevice()
    JCudaDriver.cuDeviceGet(device, 0)
    val context = new CUcontext()
    JCudaDriver.cuCtxCreate(context, 0, device)

    (device, context)
  }




}
