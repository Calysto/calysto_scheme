from IPython.kernel.zmq.kernelapp import IPKernelApp
from .kernel import CalystoScheme
IPKernelApp.launch_instance(kernel_class=CalystoScheme)
