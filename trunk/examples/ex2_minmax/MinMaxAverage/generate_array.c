#include "generate_array.h"

int make_array(int size)
{
	srand(time(NULL));

	int i=0;	
	for(i = 0;i<size;i++){
		printf("%d\n", rand()%65536);
	}

	return 0;
}
