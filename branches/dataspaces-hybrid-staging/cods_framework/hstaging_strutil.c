#include "stdio.h"
#include "string.h"

#include "hstaging_strutil.h"

static int find_char(const char *str, char c)
{
	int ret = 0;
	int i = 0;
	while (i < strlen(str)) {
		if (str[i++] == c) {
			ret = 1;
			break;
		}
	}

	return ret;
}

void trim(char *str, const char *delim)
{
	if (strlen(str) == 0) return;

	// Start
	unsigned i = 0;
	while (i < strlen(str)) {
		if (!find_char(delim, str[i]))
			break;
		i++;
	}

	if (i > 0) {
		// Erase first i chars
		size_t len = strlen(str)-i;
		if (len > 0) {
			memmove (str, str+i, len);	
		}
		str[len] = '\0';
	}

	if (strlen(str) == 0) return;

	// End
	unsigned j = strlen(str) - 1;
	while (j > 0) {
		if (!find_char(delim, str[j]))
			break;
		j--;
	}

	if (j < strlen(str)-1) {
		str[j+1] = '\0';
	}
}

void int64s_to_str(int ndim, uint64_t *values, char *s)
{
    int i;
    char v[32];
    if (!ndim) {
        s[0] = '\0';
        return;
    }
    sprintf(s,"%llu", values[0]);
    for (i=1; i<ndim; i++)
    {
        sprintf (v,",%llu", values[i]);
        strcat (s,v);
    }
}
