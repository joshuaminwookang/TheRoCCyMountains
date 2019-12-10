
#include <stdio.h>
#include <stdint.h>
#include "rocc.h"
#include "sha3.h"
#include "encoding.h"
#include "compiler.h"

#ifdef __linux
#include <sys/mman.h>
#endif

/*
 * Initializes / resets Bloom filter hardware accelerator 
 */
static inline unsigned long hw_initBloom()
{
    unsigned long rd;
	ROCC_INSTRUCTION(2, rd, rs1, 0);
	return rd ;
}

/*
 * Maps (already hashed) word to Bloom filter
 * @ params: hash value of input string to be mapped
 * @ returns: hash value of input string
 */
static inline unsigned long hw_mapToBloom(int rs1)
{
    unsigned long rd;
	ROCC_INSTRUCTION_DS(2, rd, rs1, 1);
	return rd;
}

/*
 * Tests if word is in Bloom filter
 * @ params: hash value of string to be tested against BF
 * @ returns: current miss count
 */
static inline unsigned long hw_mapToBloom(int rs1)
{
    unsigned long rd;
	ROCC_INSTRUCTION_DS(2, rd, rs1, 2);
	return rd;
}

/*
 * Test script 
 */
int main(void)
{
	unsigned long result1, result2, sum;
	result1 = ephelia_add(0,1);
    result2 = ephelia_add(0,4);
    sum = ephelia_add(1,4);

	if (sum == (result1+result2)){
        printf(" TEST Successful \n");
    } else{
	    printf(" TEST FAILED ARGHGHGHGHGHGH \n");
        return 1;
    }
    
	return 0;
}
