#include <stdio.h>
#include "FileRegister.h"
#include "ExecutionTable.h"
#include "Execute.h"
#include "Types.h"

int address;
int access;
int bit;
int data;
int updateData;
int destination;
int programCounter = 0;
int carry;
int decimalCarry;

uint32 maskTable[32] = { 	0x0, 
							0x1, 0x3, 0x7, 0xf, 
							0x1f, 0x3f, 0x7f, 0xff,
							0x1ff, 0x3ff, 0x7ff, 0xfff, 
							0x1fff, 0x3fff, 0x7fff, 0xffff, 
							0x1ffff, 0x3ffff, 0x7ffff, 0xfffff, 
							0x1fffff, 0x3fffff, 0x7fffff, 0xffffff, 
							0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff, 
							0x1fffffff, 0x3fffffff, 0x7fffffff
						};
						
uint32 getBitsAtOffset(uint32 data, int offset, int bitSize){
	
	if(offset >= 0 && bitSize > 0){
		if(offset >31)
			offset = 31;
		if(bitSize > 31)
			bitSize = 31;
		
		data = (data >> offset) & maskTable[bitSize];
		
		return data;
	}
	else
		return 0;
}

void setBitsAtOffset(uint32 *dataPtr, uint32 dataToWrite, int offset, int bitSize){

	 *dataPtr =  *dataPtr &(~(maskTable[bitSize]<<offset));
	 *dataPtr = *dataPtr|(dataToWrite<<offset);

}

int getInfo(unsigned int code){

	address = getBitsAtOffset(code,0,8);
	access = getBitsAtOffset(code,8,1);
	bit = getBitsAtOffset(code,9,3);
	destination = getBitsAtOffset(code,9,1);
	
}

int executeInstruction(unsigned int code){
	executionTable[(code & 0xFC00)>>10](code);
}


int executeDestination(int destination, int address, int access, int data){
	
	if(destination == 0){
		fileRegisters[WREG] = data;
		data = fileRegisters[WREG];
	}else{
		fileRegisters[address] = data;
		data = fileRegisters[address];
		data = setFileRegData(address, access, data);
	}
	
	return data;	
}

int executeProgramCounter(){

	programCounter = getProgramCounter();
	programCounter += 2;
	setProgramCounter(programCounter);
	
}

int executeProgramCounterSkipIfClear(int data){

	programCounter = getProgramCounter();
	
	if(data == 0){
		programCounter += 4;
	}else{
		programCounter += 2;
	}
	
	setProgramCounter(programCounter);
	
}

int executeProgramCounterSkipIfSet(int data){

	programCounter = getProgramCounter();
	
	if(data == 0){
		programCounter += 2;
	}else{
		programCounter += 4;
	}
	
	setProgramCounter(programCounter);
	
}

int checkCarryStatus(int updateData){
	if((updateData>>8) == 1){
		fileRegisters[STATUS] |= 0x01;
	}else{
		fileRegisters[STATUS] &= 0xfe;
	}
}

int checkZeroStatus(int updateData){
	if((updateData & 0xff) == 0){
		fileRegisters[STATUS] |= 0x04;
	}else{
		fileRegisters[STATUS] &= 0xfb;
	}
}

int checkNegativeStatus(int updateData){
	if((updateData>>7) == 1){
		fileRegisters[STATUS] |= 0x10;
	}else{
		fileRegisters[STATUS] &= 0xef;
	}
}
							
int executeADDWF(unsigned int code){
	//left ov and dc
	getInfo(code);

	data = getFileRegData(address,access);
	updateData = data + (fileRegisters[WREG]);
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
		
	updateData = executeDestination(destination, address, access, updateData);								
	executeProgramCounter();
	
	return updateData;
}

int withdrawCarryStatus(){
	fileRegisters[STATUS] = getBitsAtOffset(fileRegisters[STATUS],0,1);
	
	if(fileRegisters[STATUS] == 1){
		carry = 1;
	}else{
		carry = 0;
	}
	return carry;
}

int executeADDWFC(unsigned int code){

	getInfo(code);
	carry = withdrawCarryStatus();
	data = getFileRegData(address,access);
	updateData = data + (fileRegisters[WREG]) + carry;
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
		
	updateData = executeDestination(destination, address, access, updateData);								
	executeProgramCounter();
	
	return updateData;
	
	
}

int executeANDWF(unsigned int code){

	getInfo(code);

	data = getFileRegData(address,access);
	updateData = data & (fileRegisters[WREG]);
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}


int executeCLRF(unsigned int code){
	getInfo(code);

	data = getFileRegData(address,access);
	updateData = data & 0x00000000;
	
	checkZeroStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}

int executeCOMF(unsigned int code){

	getInfo(code);

	data = getFileRegData(address,access);
	updateData = ~(data);
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}


int executeCPFSEQ(unsigned int code){

	getInfo(code);
	
	programCounter = getProgramCounter();
	
	data = getFileRegData(address,access);
	if(data == fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	data = executeDestination(destination, address, access, data);
	
	return data;
}

int executeCPFSLT(unsigned int code){

	getInfo(code);
	
	programCounter = getProgramCounter();
	
	data = getFileRegData(address,access);
	if(data >= fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}

int executeCPFSGT(unsigned int code){

	getInfo(code);
	
	programCounter = getProgramCounter();
	
	data = getFileRegData(address,access);
	if(data <= fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}


int executeDECF(unsigned int code){
	//ov dc
	getInfo(code);

	data = getFileRegData(address,access);
	data -= 1;
	updateData = data;
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}

int executeDECFSZ(unsigned int code){
	getInfo(code);
	programCounter = getProgramCounter();
	data = getFileRegData(address,access);
	data -= 1;
	
	if(data == 0x00){
		programCounter +=2;
		setProgramCounter(programCounter);
	}else{
		programCounter +=4;
		setProgramCounter(programCounter);
	}
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}

int executeDCFSNZ(unsigned int code){
	getInfo(code);
	programCounter = getProgramCounter();
	data = getFileRegData(address,access);
	data -= 1;
	
	if(data != 0x00){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}


int executeINCF(unsigned int code){
	//dc ov
	getInfo(code);

	data = getFileRegData(address,access);
	data += 1;
	updateData = data;
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	return updateData;
}

int executeINCFSZ(unsigned int code){
	int value;
	getInfo(code);
	programCounter = getProgramCounter();
	data = getFileRegData(address,access);
	data += 1;
	value = data & 0x000;
	
	if(value == 0x00){
		programCounter +=2;
		setProgramCounter(programCounter);
	}else{
		programCounter +=4;
		setProgramCounter(programCounter);
	}
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}

int executeINFSNZ(unsigned int code){
	int value;
	getInfo(code);
	programCounter = getProgramCounter();
	data = getFileRegData(address,access);
	data += 1;
	value = data & 0x000;
	
	if(value != 0x00){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}
	
	data = executeDestination(destination, address, access, data);
	
	return data;
}

int executeIORWF(unsigned int code){
	getInfo(code);

	data = getFileRegData(address,access);
	updateData = data | fileRegisters[WREG];
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}

int executeMOVF(unsigned int code){
	getInfo(code);

	data = getFileRegData(address,access);
	fileRegisters[WREG]  = data;
	updateData = fileRegisters[WREG];
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = executeDestination(destination, address, access, updateData);
	executeProgramCounter();
	
	return updateData;
}

int executeMOVFF(unsigned int code){

	unsigned int sourceAddress,destAddress;

	sourceAddress = getBitsAtOffset(code, 0, 12);
	destAddress = getBitsAtOffset(code, 16, 12);
	
	fileRegisters[destAddress] = fileRegisters[sourceAddress]; 
	
	programCounter = executeProgramCounter();

	return 1;
}


int executeCPFSEQorexecuteCPFSLT(unsigned int code){
	int instruction = (code&0xff00)>>8;
	
	switch(instruction){
		case 0x62:
		executeCPFSEQ(code);
		break;
		case 0x63:
		executeCPFSEQ(code);
		break;
		case 0x61:
		executeCPFSLT(code);
		break;
		case 0x60:
		executeCPFSLT(code);
		break;
	}
}








