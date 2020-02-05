from ctypes import Structure
from ctypes import c_int
from ctypes import c_double
from ctypes import POINTER

import ctypes

class MemoryRequirements(Structure):
    _fields_ = [
        ('mxc', c_int),
        ('mxt', c_int),
        ('maxnn', c_int),
        ('maxne', c_int),
        ('ns1', c_int),
        ('kns1', c_int),
        ('maxm4', c_int),
        ('maxm5', c_int),
        ('maxeep', c_int),
        ('maxbw', c_int),
        ('maxbw2', c_int),
        ('maxm1', c_int),
        ('maxm2', c_int),
        ('maxs', c_int),
        ('mx', c_int)
    ]

class Array(Structure):
    _fields_ = [
        ('elements', POINTER(c_double)),
        ('size', c_int)
    ]

class Matrix(Structure):
    _fields_ = [
        ('elements', POINTER(POINTER(c_double))),
        ('rows', c_int),
        ('columns', c_int)
    ]
