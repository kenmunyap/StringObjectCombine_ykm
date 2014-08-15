#include <stdio.h>
#include "FileRegister.h"
#include "ExecutionTable.h"
#include "Execute.h"
#include "Types.h"



typedef unsigned int uint32;
typedef unsigned short int uint16;

typedef struct afInstruction{
 union{
	uint32 raw;
	struct{
		uint32 address:8;
		uint32 access:1;
		uint32 opcode:7;
		uint32 undefined:16;
	}fragments;
 }code;
}afInstruction;

typedef struct dafInstruction{
 union{
	uint32 raw;
	struct{
		uint32 address:8;
		uint32 access:1;
		uint32 destinationBit:1;
		uint32 opcode:6;
		uint32 undefined:16;
	}fragments;
 }code;
}dafInstruction;

typedef struct bafInstruction{
 union{
	uint32 raw;
	struct{
		uint32 address:8;
		uint32 access:1;
		uint32 bits:3;
		uint32 opcode:4;
		uint32 undefined:16;
	}fragments;
 }code;
}bafInstruction;

typedef struct ffInstruction{
 union{
	uint32 raw;
	struct{
		uint32 srcAddress:12;
		uint32 opcode:4;
		uint32 destAddress:12;
		uint32 nop:4;
	}fragments;
 }code;
}ffInstruction;

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



int executeInstruction(unsigned int code){
	executionTable[(code & 0xFC00)>>10](code);
}

int storeDestination(int destinationBit, int address, int access, int data){
	
	if(destinationBit == 0){
		fileRegisters[WREG] = data;
		data = fileRegisters[WREG];
	}else{
		fileRegisters[address] = data;
		data = fileRegisters[address];
		data = setFileRegData(address, access, data);
	}
	
	return data;	
}

/**
 *
 *	Update the program counter + 2 from previous program counter
 *
 **/
void updateProgramCounter(){
	int programCounter;
	
	programCounter = getProgramCounter();
	programCounter += 2;
	setProgramCounter(programCounter);

}

/**
 *
 *	Update the program counter + 4 from previous program counter if data is 0
 *	otherwise update program counter + 2
 *
 *	Input :
 *		data is the data after the operation
 *
 **/
void updateProgramCounterSkipIfClear(int data){
	int programCounter;
	
	programCounter = getProgramCounter();

	if(data == 0){
		programCounter += 4;
	}else{
		programCounter += 2;
	}

	setProgramCounter(programCounter);

}

/**
 *
 *	Update the program counter + 2 from previous program counter if data is 0
 *	otherwise update program counter + 4
 *
 *	Input :
 *		data is the data after the operation
 *
 **/
void updateProgramCounterSkipIfSet(int data){
	int programCounter;
	
	programCounter = getProgramCounter();

	if(data == 0){
		programCounter += 2;
	}else{
		programCounter += 4;
	}

	setProgramCounter(programCounter);

}

/**
 *
 * Input: 
 *		updateData
 *
 * For: 
 * 		Checking Carry Status
 *
 * Return:
 *		Non-return
 *
 **/
int checkCarryStatus(int updateData){
	if((updateData>>8) == 1){
		fileRegisters[STATUS] |= 0x01;
	}else{
		fileRegisters[STATUS] &= 0xfe;
	}
}

/**
 *
 * Input: 
 *		updateData
 *
 * For: 
 * 		Checking Zero Status
 *
 * Return:
 *		Non-return
 *
 **/
int checkZeroStatus(int updateData){
	if((updateData & 0xff) == 0){
		fileRegisters[STATUS] |= 0x04;
	}else{
		fileRegisters[STATUS] &= 0xfb;
	}
}

/**
 *
 * Input: 
 *		updateData
 *
 * For: 
 * 		Checking Negative Status
 *
 * Return:
 *		Non-return
 *
 **/
int checkNegativeStatus(int updateData){
	if((updateData>>7) == 1){
		fileRegisters[STATUS] |= 0x10;
	}else{
		fileRegisters[STATUS] &= 0xef;
	}
}

/**
 *
 * Input: 
 *		digitalCarry
 *
 * For: 
 * 		Checking Digital Carry Status
 *
 * Return:
 *		Non-return
 *
 **/
int checkDigitalCarryStatus(int digitalCarry){
	if(digitalCarry == 1){
		fileRegisters[STATUS] |= 0x02;
	}else{
		fileRegisters[STATUS] &= 0xfd;
	}
}

/**
 *
 * Input: 
 *		OverFlow
 *
 * For: 
 * 		Checking Over Flow
 *
 * Return:
 *		Non-return
 *
 **/
int checkOverFlow(int updataData, int overFlow){
	if((updataData >> 8) == 1){
		if(overFlow == 1){
			fileRegisters[STATUS] &= 0xf7;
		}else{
			fileRegisters[STATUS] |= 0x08;
		}
	}else{
		if(overFlow == 1){
			fileRegisters[STATUS] |= 0x08;
		}else{
			fileRegisters[STATUS] &= 0xf7;
		}
	}
}



/**
 *
 * To check pervious data that had carry status!
 *
 **/
int withdrawPerviousCarryStatus(){
	int carry;
	fileRegisters[STATUS] = getBitsAtOffset(fileRegisters[STATUS],0,1);
	
	if(fileRegisters[STATUS] == 1){
		carry = 1;
	}else{
		carry = 0;
	}
	return carry;
}

/**
 *
 *	ADD W and Carry bit to f
 *
 *	Operation : 
 *		(W) + (f) + (C) -> dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z,C,OV,DC
 *
 **/
int executeADDWFC(unsigned int code){
	dafInstruction inst;
	inst.code.raw = code;
	
	int carry;
	int digitalCarry;
	int overFlow;
	int updateData;
	int data;

	
	
	carry = withdrawPerviousCarryStatus();
	data = getFileRegData(inst.code.fragments.address,inst.code.fragments.access);
	updateData = data + (fileRegisters[WREG]) + carry;
	
	digitalCarry = (((data & 0x0f) + (fileRegisters[WREG] & 0x0f) + (carry))>>4);
	overFlow = ((data & 0x7f) + (fileRegisters[WREG] & 0x7f) + carry)>>7;
	
	checkOverFlow(updateData,overFlow);
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	checkDigitalCarryStatus(digitalCarry);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);							
	updateProgramCounter();
	
	return updateData;
	
	
}

/**
 *
 *	AND W with f
 *
 *	Operation : 
 *		(W) .AND. (f)->dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z
 *
 **/
int executeANDWF(unsigned int code){

	dafInstruction inst;
	inst.code.raw = code;
	


	int updateData;
	int data;


	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	updateData = data & (fileRegisters[WREG]);
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Clear f
 *
 *	Operation : 
 *		000h->f
 *		1->Z
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		Z
 *
 **/
int executeCLRF(unsigned int code){
	int destinationBit;
	int updateData;
	int data;
	
	afInstruction inst;
	inst.code.raw = code;
	






	data = getFileRegData(inst.code.fragments.address,inst.code.fragments.access);
	updateData = data & 0x00000000;
	
	checkZeroStatus(updateData);
	
	updateData = storeDestination(destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Complement f
 *
 *	Operation : 
 *		~(f)->dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z
 *
 **/
int executeCOMF(unsigned int code){

	dafInstruction inst;
	inst.code.raw = code;
	



	int updateData;
	int data;


	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	updateData = ~(data);
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Compare f with W, skip if f=W
 *
 *	Operation : 
 *		(f)-(W)
 *		skip if (f) = (W)
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		data
 *
 *	Status Affect:
 *		None
 *
 **/
int executeCPFSEQ(unsigned int code){

	int destinationBit;
	
	afInstruction inst;
	inst.code.raw = code;
	

	int programCounter;
	int data;
	
	programCounter = getProgramCounter();
	data = getFileRegData(inst.code.fragments.address,inst.code.fragments.access);
	if(data == fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	data = storeDestination(destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Compare f with W,skip if f<W
 *
 *	Operation : 
 *		(f)-(W),
 *		skip if (f)<(W)
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeCPFSLT(unsigned int code){

	int destinationBit;
	
	afInstruction inst;
	inst.code.raw = code;
	


	int programCounter;
	int data;


	programCounter = getProgramCounter();
	
	data = getFileRegData(inst.code.fragments.address,inst.code.fragments.access);
	if(data >= fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	
	data = storeDestination(destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Compare f with W,skip if f>W
 *
 *	Operation : 
 *		(f)-(W),
 *		skip if (f)>(W)
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeCPFSGT(unsigned int code){

	int destinationBit;
	
	afInstruction inst;
	inst.code.raw = code;
	
	int programCounter;
	int data;
	
	programCounter = getProgramCounter();
	
	data = getFileRegData(inst.code.fragments.address,inst.code.fragments.access);
	if(data <= fileRegisters[WREG]){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}	
	
	data = storeDestination(destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Decrement f
 *
 *	Operation : 
 *		(f)-1 ->dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		data
 *
 *	Status Affect:
 *		N,Z,C,OV,DC
 *
 **/
int executeDECF(unsigned int code){

	dafInstruction inst;
	inst.code.raw = code;
	

	int digitalCarry;
	int overFlow;
	int programCounter;
	int updateData;
	int data;


	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	digitalCarry = ((data & 0x0f) +((~(0x01)+1) & 0x0f)>>4);
	overFlow = ((data & 0x7f) +((~(0x01)+1) & 0x7f))>>7;
	data -= 1;
	updateData = data;
	
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	checkDigitalCarryStatus(digitalCarry);
	checkOverFlow(updateData,overFlow);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Decrement f,skip if 0
 *
 *	Operation : 
 *		(f) - 1 ->dest
 * 		skip if result = 0
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeDECFSZ(unsigned int code){

	dafInstruction inst;
	inst.code.raw = code;
	



	int programCounter;
	int data;

	
	programCounter = getProgramCounter();
	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	data -= 1;
	
	if(data == 0x00){
		programCounter +=2;
		setProgramCounter(programCounter);
	}else{
		programCounter +=4;
		setProgramCounter(programCounter);
	}
	
	data = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Decrement f, skip if not 0
 *
 *	Operation : 
 *		(f) - 1 ->dest
 *      skip if result != 0
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeDCFSNZ(unsigned int code){
	dafInstruction inst;
	inst.code.raw = code;
	
	int programCounter;
	int data;

	
	programCounter = getProgramCounter();
	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	data -= 1;
	
	if(data != 0x00){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}
	
	data = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Increment f
 *
 *	Operation : 
 *		(f) + 1 -> dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z,C,DC,OV
 *
 **/
int executeINCF(unsigned int code){
	dafInstruction inst;
	inst.code.raw = code;
	

	int digitalCarry;
	int overFlow;
	int updateData;
	int data;


	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	digitalCarry = (((data & 0x0f) + 1)>>4);
	overFlow = (((data & 0x7f) + 1))>>7;
	data += 1;
	updateData = data;
	
	checkCarryStatus(updateData);
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	checkDigitalCarryStatus(digitalCarry);
	checkOverFlow(updateData,overFlow);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	return updateData;
}

/**
 *
 *	Increment f, skip if 0
 *
 *	Operation : 
 *		(f) + 1 ->dest
 *		skip if result = 0
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeINCFSZ(unsigned int code){
	int value;
	
	dafInstruction inst;
	inst.code.raw = code;
	

	int programCounter;
	int data;

	
	programCounter = getProgramCounter();
	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	data += 1;
	value = data & 0x000;
	
	if(value == 0x00){
		programCounter +=2;
		setProgramCounter(programCounter);
	}else{
		programCounter +=4;
		setProgramCounter(programCounter);
	}
	
	data = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Increment f,skip if not 0
 *
 *	Operation : 
 *		(f) + 1 ->dest
 *		skip if result != 0
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		None
 *
 **/
int executeINFSNZ(unsigned int code){
	int value;
	dafInstruction inst;
	inst.code.raw = code;
	

	int programCounter;
	int data;

	
	programCounter = getProgramCounter();
	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	data += 1;
	value = data & 0x000;
	
	if(value != 0x00){
		programCounter +=4;
		setProgramCounter(programCounter);
	}else{
		programCounter +=2;
		setProgramCounter(programCounter);
	}
	
	data = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, data);
	
	return data;
}

/**
 *
 *	Inclusive OR W with f
 *
 *	Operation : 
 *		(W).OR.(f) -> dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z
 *
 **/
int executeIORWF(unsigned int code){
	dafInstruction inst;
	inst.code.raw = code;
	




	int updateData;
	int data;


	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	updateData = data | fileRegisters[WREG];
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Move f
 *
 *	Operation : 
 *		F->dest
 *
 *	Input :
 *		code is the opcode for instruction word
 *	
 *	Return :
 *		updateData
 *
 *	Status Affect:
 *		N,Z
 *
 **/
int executeMOVF(unsigned int code){
	dafInstruction inst;
	inst.code.raw = code;
	
	int updateData;
	int data;

	data = getFileRegData(inst.code.fragments.address, inst.code.fragments.access);
	fileRegisters[WREG]  = data;
	updateData = fileRegisters[WREG];
	
	checkZeroStatus(updateData);
	checkNegativeStatus(updateData);
	
	updateData = storeDestination(inst.code.fragments.destinationBit, inst.code.fragments.address, inst.code.fragments.access, updateData);
	updateProgramCounter();
	
	return updateData;
}

/**
 *
 *	Move source file to destination file
 *
 *	Operation : 
 *		Fs->Fd
 *
 *	Input :
 *		code is the opcode for instruction word
 *
 *	Status Affect:
 *		None
 *
 **/
int executeMOVFF(unsigned int code){
	
	unsigned int sourceAddress,destAddress;
	ffInstruction inst;
	inst.code.raw = code;
	
	sourceAddress = getBitsAtOffset(code, 0, 12);
	destAddress = getBitsAtOffset(code, 16, 12);
	
	fileRegisters[destAddress] = fileRegisters[sourceAddress]; 
	
	updateProgramCounter();

	return 1;
}

/**
 *
 * To consider CPFSEQ or CPFSLT instruction to be taken!
 *
 **/
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








