# -*- coding: utf-8 -*-
# @Author: yow004
# @Date:   2019-01-25 12:53:04
# @Last Modified by:   yow004
# @Last Modified time: 2019-01-25 13:09:33
notes = """
account: jmz
"""
login = 'wangyf@theta.acf.anl.gov'
hosts = 'h2ologin3',
maxcores = 64
maxram = 6553600 
minnodes = 1
maxnodes = 8640 
maxtime = 24, 00
rate = 1e6
fortran_serial = 'ifort',
fortran_mpi = 'ftn',
#fortran_serial = 'gfortran',
#fortran_mpi = 'mpif90',
#_ = '-fimplicit-none -fcheck=all -g -fbacktrace', '-o'
_ = '','-o'
fortran_flags = {
    'g': ('-Ktrap=fp', '-Mbounds', '-Mchkptr', '-g') + _,
    't': ('-Ktrap=fp', '-Mbounds') + _,
    'p': ('-pg', '-Mprof=func') + _,
#    'O': ('-fast -vec-report -no-ipo -g ',) + _,
#    'O': ('-O3 -ffast-math -funroll-loops -Wall -cpp -Ddoubleprecision -fdefault-real-8',) + _,
    'O': ('-O3 -xMIC-AVX512','-fpp -Ddoubleprecision=1','-real-size 64') + _,
}

