#include "unity.h"
#include "FileRegister.h"
#include "ExecutionTable.h"
#include "Execute.h"
#include "Types.h"

void setUp(void){}
void tearDown(void){}

void test_setBitsAtOffset(){
	int value = 0x1E36;
	setBitsAtOffset(&value,0x15,4,5);
	
	TEST_ASSERT_EQUAL_HEX32(0x1F56,value);
}


/*
*  checking ADDWFC
*/
void test_executeADDWFC_is_in_FileRegister_and_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[WREG] = 0x89;
	fileRegisters[0xf31] = 0x19;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	fileRegisters[STATUS] = 0x01;
	
	code = 0x2331;
	data = executeInstruction(code);
	

	
	TEST_ASSERT_EQUAL(0xA3, data);
	TEST_ASSERT_EQUAL(0x89, fileRegisters[WREG]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x12, fileRegisters[STATUS]);
}

/*
*	checking ANDWF
*/
void test_executeANDWF_is_in_FileRegister_and_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[WREG] = 0x01;
	fileRegisters[0xf31] = 0x01;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x1731;
	data = executeInstruction(code);
	

	
	TEST_ASSERT_EQUAL(0x01, data);
	TEST_ASSERT_EQUAL(0x01, fileRegisters[WREG]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[STATUS]);	
}

void test_executeANDWF_is_in_FileRegister_and_ACCESS(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[WREG] = 0xA1;
	fileRegisters[0x31] = 0x90;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;

	code = 0x1731;
	data = executeInstruction(code);
	
		
	TEST_ASSERT_EQUAL(0x80, data);
	TEST_ASSERT_EQUAL(0xA1, fileRegisters[WREG]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x10, fileRegisters[STATUS]);
}

/*
*	checking CLRF
*/
void test_executeCLRF_is_in_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x1A;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x6A31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x04, fileRegisters[STATUS]);
}

void test_executeCLRF_is_in_ACCESS(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	
	fileRegisters[0x31] = 0x21;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;

	code = 0x6A31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x04, fileRegisters[STATUS]);
}

/*
*	checking COMF
*/
void test_executeCOMF_is_in_FileRegister_and_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x13;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x1F31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0xEC, data);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[STATUS]);
}

void test_executeCOMF_is_in_FileRegister_and_ACCESS(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[0x31] = 0xFF;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	printf("%x \n",data);
	printf("%x",fileRegisters[STATUS]);
	
	code = 0x1F31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[PCLATU]); 
	TEST_ASSERT_EQUAL(0x03, fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x04, fileRegisters[STATUS]);
}

/*
*	checking CPFSEQ
*/
void test_executeCPFSEQ_is_in_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x1A;
	fileRegisters[WREG] = 0x1A;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x6331;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x1A, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x05,fileRegisters[PCL]);
}


void test_executeCPFSGT_is_in_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x1A;
	fileRegisters[WREG] = 0x2A;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x6531;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x1A, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x05,fileRegisters[PCL]);
}

void test_executeCPFSLT_is_in_BANKED(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x2A;
	fileRegisters[WREG] = 0x1A;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x6131;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x2A, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x05,fileRegisters[PCL]);
}

void test_executeDECF_0x11_no_status(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x11;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x0731;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x10, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x0A, fileRegisters[STATUS]);
}

void test_executeDECF_0x01_zero_status(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x01;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x0731;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x0E, fileRegisters[STATUS]);
}



void test_executeDECFSZ_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x11;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x2F31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x10, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x05,fileRegisters[PCL]);
}

void test_executeDCFSNZ_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x11;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x4F31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x10, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x05,fileRegisters[PCL]);

}

void test_executeINCF(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x1F;	
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;
	
	code = 0x2B31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x20, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x02, fileRegisters[STATUS]);
}

void test_executeINCFSZ_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0xff;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x3F31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
}

void test_executeINFSNZ_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0xff;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x4B31;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x00, data);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
}

void test_executeIORWF_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x13;
	fileRegisters[WREG] = 0x91;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x1331;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x93, data);
	TEST_ASSERT_EQUAL(0x91,fileRegisters[WREG]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x10, fileRegisters[STATUS]);
}

void test_executeMOVF_BANKED_and_File_Register(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);
	fileRegisters[BSR] = 0xf;
	fileRegisters[0xf31] = 0x13;
	fileRegisters[WREG] = 0x91;
	fileRegisters[PCLATU] = 0x00;
	fileRegisters[PCLATH] = 0x00;
	fileRegisters[PCL] = 0x01;	

	code = 0x5331;
	data = executeInstruction(code);
	
	TEST_ASSERT_EQUAL(0x13, data);
	TEST_ASSERT_EQUAL(0x13,fileRegisters[WREG]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATU]);
	TEST_ASSERT_EQUAL_HEX16(0x00,fileRegisters[PCLATH]);
	TEST_ASSERT_EQUAL_HEX16(0x03,fileRegisters[PCL]);
	TEST_ASSERT_EQUAL(0x00, fileRegisters[STATUS]);
}

void test_executeMOVFF(){
	int code;
	int data;
	clearAllFileRegisters(fileRegisters);

	fileRegisters[0x131] = 0x13;
	fileRegisters[0x132] = 0x91;
	
	code = 0xF132C131;
	
	data = executeInstruction(code);
		
	
	TEST_ASSERT_EQUAL(1, data);
	TEST_ASSERT_EQUAL(0x13, fileRegisters[0x131]);
	TEST_ASSERT_EQUAL(0x13, fileRegisters[0x132]);
}













