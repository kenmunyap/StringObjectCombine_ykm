#include <stdio.h>
#include "FileRegister.h"
#include "ExecutionTable.h"
#include "Execute.h"
#include "Types.h"

int address;
int access;
int bit;
int data;
int destination;
int programCounter = 0;
int carry;


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


void setNegativeFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] |= 0x10;
}

void clearNegativeFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] &= 0xef;
}

void setOverFlowFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] |= 0x08;
}

void clearOverFlowFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] &= 0xf7;
}

void setZeroFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] |= 0x04;
}

void clearZeroFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] &= 0xfb;
}

void setDigitalCarryFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] |= 0x02;
}

void clearDigitalCarryFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] &= 0xfd;
}

void setCarryFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] |= 0x01;
}

void clearCarryFlag(){
	// - - - N OV Z DC C
	fileRegisters[STATUS] &= 0xfe;
}


int checkStatus(int data){
	// - - - N OV Z DC C
	if(data == 0x0){
		fileRegisters[STATUS] = 0x04;	// Zero
	}else if(data < 0x0){
		fileRegisters[STATUS] = 0x10;	// Negative
	}else if((data & 0x01) == 1){
		fileRegisters[STATUS] = 0x01;	// Carry
	}else{
		fileRegisters[STATUS] = 0x0;	// OV and DC
	}
	
	return fileRegisters[STATUS];
}




int getInfo(unsigned int code){

	address = code & 0xff;
	access = ((code & 0x100)>>8);
	bit = ((code & 0xE00)>>9);
	destination = ((code & 0x200)>>9);
	
}

int executeInstruction(unsigned int code){

	executionTable[(code & 0xFC00)>>10](code);

}



int executeCarryStatus(){
	
	fileRegisters[STATUS] = fileRegisters[STATUS] & 0x01;
	if(fileRegisters[STATUS] == 1){
		carry = 0;
	}else if(fileRegisters[STATUS] == 0){
		carry = 1;
	}
	
	return carry;
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
								 //N 4bit
								 //OV 3b
								 //Z 2b
								 //DC 1b
								 //C 0b
int executeADDWF(unsigned int code){

	getInfo(code);

	data = getFileRegData(address,access);
	data = data + (fileRegisters[WREG]);
	
	 if(((data & 0x10)>>4) == 1){
		setNegativeFlag();
	 }else{
		clearNegativeFlag();
	 }
	 
	 if(((data & 0x08)>>3) == 1){
		setOverFlowFlag();
	 }else{
		clearOverFlowFlag();
	 }
	 
	 if(((data & 0x04)>>2) == 1){
		setZeroFlag();
	 }else{
		clearZeroFlag();
	 }
	 
	 if(((data & 0x02)>>1) == 1){
		setDigitalCarryFlag();
	 }else{
		clearDigitalCarryFlag();
	 }
	 
	 if((data) == 1){
		setCarryFlag();
	 }else{
		clearCarryFlag();
	 }
	
	data = executeDestination(destination, address, access, data);
	

	 
								
	programCounter = executeProgramCounter();
	
	return data;
}

int executeANDWF(unsigned int code){

	getInfo(code);

	data = getFileRegData(address,access);
	data = data & (fileRegisters[WREG]);
	
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	return data;
}

int executeCLRF(unsigned int code){
	
	getInfo(code);

	data = getFileRegData(address,access);
	data = data & 0x00000000;
	
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	
	return data;
}

int executeCOMF(unsigned int code){

	getInfo(code);

	data = getFileRegData(address,access);
	data = ~(data);
	
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	
	return data;
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

int executeDECF(unsigned int code){
	getInfo(code);

	data = getFileRegData(address,access);
	data -= 1;
	
	data = executeDestination(destination, address, access, data);
	programCounter = executeProgramCounter();
	return data;
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
	getInfo(code);

	data = getFileRegData(address,access);
	data += 1;
	
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	return data;
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
	data = data | fileRegisters[WREG];
	
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	return data;
}

int executeMOVF(unsigned int code){
	getInfo(code);

	data = getFileRegData(address,access);
	fileRegisters[WREG]  = data;
	data = fileRegisters[WREG];
	data = executeDestination(destination, address, access, data);
	
	programCounter = executeProgramCounter();
	return data;
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








