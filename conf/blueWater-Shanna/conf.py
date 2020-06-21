# -*- coding: utf-8 -*-
# @Author: yow004
# @Date:   2019-01-25 12:53:04
# @Last Modified by:   yow004
# @Last Modified time: 2019-01-25 13:09:33
notes = """
account: jmz
"""
login = 'h2ologin.ncsa.illinois.edu'
hosts = 'h2ologin3',
maxcores = 16
maxram = 65536 
minnodes = 1
maxnodes = 22640 
maxtime = 24, 00
rate = 1e6
fortran_serial = 'gfortran',
fortran_mpi = 'mpif90',
_ = '', '-o'
fortran_flags = {
    'g': ('-Ktrap=fp', '-Mbounds', '-Mchkptr', '-g') + _,
    't': ('-Ktrap=fp', '-Mbounds') + _,
    'p': ('-pg', '-Mprof=func') + _,
#    'O': ('-fast -vec-report -no-ipo -g ',) + _,
    'O': ('-O3 -ffast-math -funroll-loops',) + _,
}

