notes = """
local machine (MacBook Pro)
"""
login = ''
hosts = '',
maxnodes = 500
maxcores = 56
maxram = 30000
fortran_serial = 'gfortran',
fortran_mpi = 'mpif90',
_ ='-fcheck=all', '-cpp','-fdefault-real-8','-Ddoubleprecision','-fimplicit-none', '-Wall -Wno-maybe-uninitialized', '-o'
fortran_flags = {
    'g': ('-fbounds-check', '-ffpe-trap=invalid,zero,overflow', '-g') + _,
    't': ('-fbounds-check', '-ffpe-trap=invalid,zero,overflow') + _,
    'p': ('-O', '-pg') + _,
    'O': ('-O3',) + _,
}

