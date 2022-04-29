#include "freertos/semphr.h"

#ifndef _LEDZ_H
#define _LEDZ_H

#define PAUSE_TASK 0
#define RESUME_TASK 1

#define MAX_CODE_SIZE 16 * 1024

extern TaskHandle_t ledzTaskHandle;
extern SemaphoreHandle_t ledzTaskMutex;
extern int programCode[MAX_CODE_SIZE];

void ledz_task(void* args);

#endif // _COLOR_H
