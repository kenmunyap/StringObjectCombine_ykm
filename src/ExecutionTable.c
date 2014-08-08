#include <stdio.h>
#include "ExecutionTable.h"
#include "FileRegister.h"
#include "Execute.h"

ExecutionTable executionTable[64] = {
	[0x09] = executeADDWF,
	[0x05] = executeANDWF,
	[0x1A] = executeCLRF,
	[0x07] = executeCOMF,
	[0x18] = executeCPFSEQorexecuteCPFSLT,
	[0x19] = executeCPFSGT,
	[0x01] = executeDECF,
	[0x0B] = executeDECFSZ,
	[0x13] = executeDCFSNZ,
	[0x0A] = executeINCF,
	[0x0F] = executeINCFSZ,
	[0x12] = executeINFSNZ,
	[0x04] = executeIORWF,
	[0x14] = executeMOVF,
	[0x30] = executeMOVFF,
	[0x31] = executeMOVFF,
	[0x32] = executeMOVFF,
	[0x33] = executeMOVFF,
};