import sys
import numpy as np

# global variable
idtype = np.dtype( 'f8' ).str #'<f8'

def readbin(file,shape=None,inputdtype=None):
	fd = open( file ,'rb')
	if inputdtype == None:
		inputdtype = idtype
	if shape==None:
		matrix = np.fromfile(fd, inputdtype).astype(idtype)
	else:
		matrix = np.fromfile(fd, inputdtype).reshape(shape).astype(idtype)
	print('Read size check',file,matrix.shape)
	fd.close()
	return matrix

if __name__ == '__main__':

	#filename
	file = sys.argv[1]

	data = readbin(file)

	print('File:{0} will be check'.format(file))

	for i in range(len(data)):
		print('{0}'.format(data[i]))

	print('Check complete')
