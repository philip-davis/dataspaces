#include <stdio.h>
#include <string.h>
#include "CppWrapper.h"

int main() {
        WrapperMap *t = NULL;

        t = map_new(5);
        const char * pred = "a";
        const char * succ = "b";
        const char * rt_value = "absuchbasckj";
        printf("%s\n", rt_value);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        succ = "d";
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        pred = "ab";
        succ = "c";
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        map_insert(t, pred, succ);
        pred = "ab";
        rt_value = map_get_value(t, pred);
        printf("%s\n", rt_value);
        pred = "a";
        rt_value = map_get_value(t, pred);
        printf("%s\n", rt_value);
        pred = "ac";
        rt_value = map_get_value(t, pred);
        printf("%s\n", rt_value);
        map_delete(t);
        t = NULL;

        return 0;
}