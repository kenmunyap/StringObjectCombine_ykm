#ifndef _EXECUTE_H_
#define _EXECUTE_H_
#include "Types.h"
#define	WREG 	0xFE8

uint32 getBitsAtOffset(uint32 data, int offset, int bitSize);

int getInfo(unsigned int code);
int executeInstruction(unsigned int code);
int executeCarryStatus();
int checkStatus(int data);
int storeDestination(int destination, int address, int access, int data);
int executeProgramCounter();
int executeProgramCounterSkipIfClear(int data);
int executeProgramCounterSkipIfSet(int data);
int checkCarryStatus(int updateData);
int checkZeroStatus(int updateData);
int checkNegativeStatus(int updateData);
int withdrawCarryStatus();
int checkDigitalCarryStatus(int digitalCarry);
int checkOverFlow(int updataData, int overFlow);

int executeADDWF(unsigned int code);
int executeADDWFC(unsigned int code);
int executeANDWF(unsigned int code);
int executeCLRF(unsigned int code);
int executeCOMF(unsigned int code);
int executeCPFSEQorexecuteCPFSLT(unsigned int code);
int executeCPFSGT(unsigned int code);
int executeCPFSEQ(unsigned int code);
int executeCPFSLT(unsigned int code);
int executeDECF(unsigned int code);
int executeDECFSZ(unsigned int code);
int executeDCFSNZ(unsigned int code);
int executeINCF(unsigned int code);
int executeINCFSZ(unsigned int code);
int executeINFSNZ(unsigned int code);
int executeIORWF(unsigned int code);
int executeMOVF(unsigned int code);
int executeMOVFF(unsigned int code);
#endif //_EXECUTE_H_