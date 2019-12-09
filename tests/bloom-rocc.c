
#include <stdio.h>
#include <stdint.h>
#include "rocc.h"
#include "sha3.h"
#include "encoding.h"
#include "compiler.h"

#ifdef __linux
#include <sys/mman.h>
#endif


static inline unsigned long hw_mapToBloom(int rs1)
{
    unsigned long rd;
	ROCC_INSTRUCTION_DSS(2, rd, rs1, rs2, 0);
	return rd;
}


// unsigned long data = 0x3421L;

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
