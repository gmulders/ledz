#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_log.h"
#include "driver/rmt.h"
#include "esp_timer.h"
#include "color.h"
#include "ledz.h"
#include "vm.h"
#include "string.h"

static const char *TAG = "ledz";

#define LED_COUNT (470) // 2 * (3 * 72 + 19)
#define BYTES_PER_LED 4
#define BITS_PER_LED (BYTES_PER_LED * 8)

static const float pulse_width = 12.5;
static const uint8_t clock_divider = 4;

// These are the timings for WS2812, which are useless, because this piece of software is RGBW only
// #define T0H 400
// #define T0L 850
// #define T1H 800
// #define T1L 450
// #define TRST 55000

// These are the tweaked timings for SK6812RGBW
#define T0H 300	
#define T0L 850
#define T1H 600
#define T1L 450
#define TRST 55000

// These are the official timings for SK6812RGBW
// #define T0H 300	
// #define T0L 900
// #define T1H 600
// #define T1L 600
// #define TRST 80000

static const rmt_item32_t lo = {
    .duration0 = (T0H / (clock_divider * pulse_width)),
    .level0 = 1,
    .duration1 = (T0L / (clock_divider * pulse_width)),
    .level1 = 0,
};

static const rmt_item32_t hi = {
    .duration0 = (T1H / (clock_divider * pulse_width)),
    .level0 = 1,
    .duration1 = (T1L / (clock_divider * pulse_width)),
    .level1 = 0,
};

static const rmt_item32_t reset = {
    .duration0 = (TRST / (clock_divider * pulse_width)),
    .level0 = 0,
    .duration1 = 0,
    .level1 = 0,
};

static void rmt_adapter(const void *src, rmt_item32_t *dest, size_t src_size, size_t wanted_num, size_t *translated_size, size_t *item_num)
{
    if (src == NULL || dest == NULL)
    {
        *translated_size = 0;
        *item_num = 0;
        return;
    }
    size_t size = 0;
    size_t num = 0;
    uint8_t *psrc = (uint8_t *)src;
    rmt_item32_t *pdest = dest;
    while (size < src_size && num < wanted_num)
    {
        for (int i = 0; i < 8; i++)
        {
            // MSB first
            if (*psrc & (1 << (7 - i)))
            {
                pdest->val = hi.val;
            }
            else
            {
                pdest->val = lo.val;
            }
            num++;
            pdest++;
        }
        size++;
        psrc++;
    }
    *translated_size = size;
    *item_num = num;
}

/*
 * Initialize the RMT Tx channel
 */
static void rmt_tx_init(void)
{
    rmt_config_t config = {
        .rmt_mode = RMT_MODE_TX,
        .channel = RMT_CHANNEL_0,
        .gpio_num = 19,
        .clk_div = 4, // 80
        .mem_block_num = 3, // 1
        .flags = 0,
        .tx_config = {
            .carrier_freq_hz = 38000,
            .carrier_level = RMT_CARRIER_LEVEL_HIGH,
            .idle_level = RMT_IDLE_LEVEL_LOW,
            .carrier_duty_percent = 33,
            .carrier_en = false,
            .loop_en = false,
            .idle_output_en = true,
        }
    };

    ESP_ERROR_CHECK(rmt_config(&config));
    ESP_ERROR_CHECK(rmt_driver_install(config.channel, 0, 0));
    ESP_ERROR_CHECK(rmt_translator_init(config.channel, rmt_adapter));
}

#define PROGRAM_RAINBOW 0
#define PROGRAM_WHITE 1

static color_t *led_data_buffer;

static int getR(VM *vm) {
	int sp = vm->sp;
	int i = vm->stack[sp--];
	vm->stack[++sp] = led_data_buffer[i].r;
	vm->sp = sp;
	return 1;
}

static int setR(VM *vm) {
	int sp = vm->sp;
	int r = vm->stack[sp--];
	int i = vm->stack[sp--];
	led_data_buffer[i].r = r & 0xff;
	vm->stack[++sp] = 1;
	vm->sp = sp;
	return 1;
}

static int getG(VM *vm) {
	int sp = vm->sp;
	int i = vm->stack[sp--];
	vm->stack[++sp] = led_data_buffer[i].g;
	vm->sp = sp;
	return 1;
}

static int setG(VM *vm) {
	int sp = vm->sp;
	int g = vm->stack[sp--];
	int i = vm->stack[sp--];
	led_data_buffer[i].g = g & 0xff;
	vm->stack[++sp] = 1;
	vm->sp = sp;
	return 1;
}

static int getB(VM *vm) {
	int sp = vm->sp;
	int i = vm->stack[sp--];
	vm->stack[++sp] = led_data_buffer[i].b;
	vm->sp = sp;
	return 1;
}

static int setB(VM *vm) {
	int sp = vm->sp;
	int b = vm->stack[sp--];
	int i = vm->stack[sp--];
	led_data_buffer[i].b = b & 0xff;
	vm->stack[++sp] = 1;
	vm->sp = sp;
	return 1;
}

static int getW(VM *vm) {
	int sp = vm->sp;
	int i = vm->stack[sp--];
	vm->stack[++sp] = led_data_buffer[i].w;
	vm->sp = sp;
	return 1;
}

static int setW(VM *vm) {
	int sp = vm->sp;
	int w = vm->stack[sp--];
	int i = vm->stack[sp--];
	led_data_buffer[i].w = w & 0xff;
	vm->stack[++sp] = 1;
	vm->sp = sp;
	return 1;
}

#define POP_FLOAT			(*((float*)(vm->stack + sp--)))
#define PUSH_FLOAT(val)		(vm->stack[++sp] = *((int*)(&(val))))
static int powFn(VM *vm) {
	int sp = vm->sp;
	float e = POP_FLOAT;
	float b = POP_FLOAT;
    float p = pow(b, e);
	PUSH_FLOAT(p);
	vm->sp = sp;
	return 1;
}

static int expFn(VM *vm) {
	int sp = vm->sp;
	float e = POP_FLOAT;
    float p = exp(e);
	PUSH_FLOAT(p);
	vm->sp = sp;
	return 1;
}

// #define START_PROGRAM_SIZE 38
// int start_program[START_PROGRAM_SIZE] = {
// 	Call, 7, 2, 1, 0, 0,                    // 0
// 	Halt,                                   // 6
// 	IntConst, 0,                            // 7
// 	Store, 1,                               // 9
// 	Load, 1,                                // 11
// 	Load, -6,                               // 13
// 	IntLt,                                  // 15
// 	BranchFalse, 16,                        // 16
// 	Load, 1,                                // 18
// 	IntConst, 15,                           // 20
// 	CallIn, 7,                              // 22
// 	Pop,                                    // 24
// 	Load, 1,                                // 25
// 	IntConst, 1,                            // 27
// 	IntAdd,                                 // 29
// 	Store, 1,                               // 30
// 	Branch, -23,                            // 32
// 	IntConst, 0,                            // 34
// 	Return, 1,                              // 36
// };

#define START_PROGRAM_SIZE 172
	int start_program[START_PROGRAM_SIZE] = {
    DupX1,                                  // 0
    BranchTrue, 8,                          // 1
    Dup,                                    // 3
    Call, 19, 1, 2, 0, 0,                   // 4
    Pop,                                    // 10
    Swap,                                   // 11
    Call, 106, 2, 2, 0, 0,                  // 12
    Halt,                                   // 18
    Load, -5,                               // 19
    NewArray,                               // 21
    GlobalStoreObject, 0,                   // 22
    Load, -5,                               // 24
    IntConst, 2,                            // 26
    IntDiv,                                 // 28
    GlobalStore, 0,                         // 29
    GlobalLoad, 0,                          // 31
    IntConst, 2,                            // 33
    IntDiv,                                 // 35
    Store, 1,                               // 36
    IntConst, 0,                            // 38
    Store, 2,                               // 40
    Load, 2,                                // 42
    GlobalLoad, 0,                          // 44
    IntLt,                                  // 46
    BranchFalse, 30,                        // 47
    GlobalLoadObject, 0,                    // 49
    Load, 2,                                // 51
    Load, 1,                                // 53
    IntSub,                                 // 55
    Load, 2,                                // 56
    Load, 1,                                // 58
    IntSub,                                 // 60
    IntMul,                                 // 61
    IntNeg,                                 // 62
    Int2Float,                              // 63
    FloatConst, 1073741824,                 // 64
    FloatDiv,                               // 66
    Load, 2,                                // 67
    ArrayStore,                             // 69
    Load, 2,                                // 70
    IntConst, 1,                            // 72
    IntAdd,                                 // 74
    Store, 2,                               // 75
    Branch, -37,                            // 77
    Load, 2,                                // 79
    Load, -5,                               // 81
    IntLt,                                  // 83
    BranchFalse, 16,                        // 84
    Load, 2,                                // 86
    IntConst, 16,                           // 88
    CallIn, 7,                              // 90
    Pop,                                    // 92
    Load, 2,                                // 93
    IntConst, 1,                            // 95
    IntAdd,                                 // 97
    Store, 2,                               // 98
    Branch, -23,                            // 100
    IntConst, 0,                            // 102
    Return, 1,                              // 104
    FloatConst, 1176256512,                 // 106
    FloatConst, 1045220557,                 // 108
    Load, -5,                               // 110
    Int2Float,                              // 112
    FloatConst, 1050253722,                 // 113
    CallIn, 8,                              // 115
    CallIn, 8,                              // 117
    FloatMul,                               // 119
    Store, 1,                               // 120
    Load, 1,                                // 122
    Load, 1,                                // 124
    FloatMul,                               // 126
    Store, 1,                               // 127
    IntConst, 0,                            // 129
    Store, 2,                               // 131
    Load, 2,                                // 133
    GlobalLoad, 0,                          // 135
    IntLt,                                  // 137
    BranchFalse, 28,                        // 138
    Load, 2,                                // 140
    FloatConst, 1132396544,                 // 142
    GlobalLoadObject, 0,                    // 144
    Load, 2,                                // 146
    ArrayLoad,                              // 148
    Load, 1,                                // 149
    FloatDiv,                               // 151
    CallIn, 9,                              // 152
    FloatMul,                               // 154
    Float2Int,                              // 155
    CallIn, 1,                              // 156
    Pop,                                    // 158
    Load, 2,                                // 159
    IntConst, 1,                            // 161
    IntAdd,                                 // 163
    Store, 2,                               // 164
    Branch, -35,                            // 166
    IntConst, 0,                            // 168
    Return, 1,                              // 170
};

static internal_function fns[10] = { getR, setR, getG, setG, getB, setB, getW, setW, powFn, expFn };

// static VM *_vm_create(int *code, int code_size) {
// 	int *new_code = malloc(code_size * sizeof(int));
// 	memcpy(new_code, code, code_size * sizeof(int));
// 	return vm_create(new_code, code_size, 1024, 128, 64 * 1024, fns, 8);
// }

// static void _vm_free(VM *vm) {
// 	free(vm->code);
// 	vm_free(vm);
// }

static void _vm_exec(VM *vm, int counter) {
	vm->stack[0] = LED_COUNT;
	vm->stack[1] = counter;
	vm_exec(vm, 0, 1);
}

static void clear_led_data_buffer()
{
    for (int l = 0; l < LED_COUNT; l++) {
        led_data_buffer[l] = color_from_rgbw(0, 0, 0, 0);
    }
}

void ledz_task(void* args)
{
    ESP_LOGI(TAG, "Configuring transmitter");
    rmt_tx_init();

    // 4 bytes per LED (r, g, b, w)
    led_data_buffer = malloc(LED_COUNT * sizeof(int));
    clear_led_data_buffer();

	while (xSemaphoreTake(ledzTaskMutex, pdMS_TO_TICKS(250)) != pdTRUE) {
		ESP_LOGE(TAG, "Could not get lock to vm; retry");
	}
	memcpy(programCode, start_program, START_PROGRAM_SIZE * 4);
	VM *vm = vm_create(programCode, MAX_CODE_SIZE, 1024, 128, 64 * 1024, fns, 8);
	
	int paused = 0;
    int i = 0;
    int64_t time = esp_timer_get_time();
	TickType_t block_time = pdMS_TO_TICKS(0);
	uint32_t code;

	ESP_LOGI(TAG, "Starting loop");
    while (1) {
		// Check if there is a new program waiting.
		if (xTaskNotifyWait(pdFALSE, 0x00, &code,  block_time) == pdPASS) {
		    ESP_LOGI(TAG, "Command received");
			if (code == PAUSE_TASK) {
			    ESP_LOGI(TAG, "Pausing Task");
				// Release lock
				xSemaphoreGive(ledzTaskMutex);
				// free vm
				paused = 1;
			} else if (code == RESUME_TASK) {
			    ESP_LOGI(TAG, "Resuming Task");
				// Get lock
				if (xSemaphoreTake(ledzTaskMutex, pdMS_TO_TICKS(250)) != pdTRUE) {
					ESP_LOGE(TAG, "Could not get lock to vm");
					continue;
				}
                clear_led_data_buffer();
				i = 0;
				paused = 0;
			} else {
				ESP_LOGI(TAG, "Unknown command");
			}
		}

		if (paused) {
			continue;
		}

		// Run het programma
		_vm_exec(vm, i);

        // Pak het programma en vul de led_data_buffer en de xDelay
        // for (int l = 0; l < LED_COUNT; l++) {
        //     color_t c;
        //     switch (currentProgram)
        //     {
        //     case PROGRAM_WHITE:
        //         c = color_from_rgbw(0, 0, 0, i & 0xff);
        //         break;

        //     case PROGRAM_RAINBOW:
        //         c = rainbow(l, i);
        //         break;

        //     default:
        //         c = color_from_rgbw(0, 0, 0, 0);
        //         break;
        //     }

		// 	led_data_buffer[l] = c;
        //     // led_data_buffer[l * 4 + 0] = c.r;
        //     // led_data_buffer[l * 4 + 1] = c.g;
        //     // led_data_buffer[l * 4 + 2] = c.b;
        //     // led_data_buffer[l * 4 + 3] = c.w;
        // }
        ESP_ERROR_CHECK(rmt_write_sample(RMT_CHANNEL_0, (uint8_t *)led_data_buffer, LED_COUNT * 4, true));
        ESP_ERROR_CHECK(rmt_write_items(RMT_CHANNEL_0, &reset, 1, true));

        i += 1;

        // if ((i &= 0xff) == 0) {
		// 	int64_t now = esp_timer_get_time();
        //     int64_t diff = now - time;
		// 	float freq = 256.0 / (diff / 1000000.0);
		// 	float maxFreq = 1.0 / ((T0H + T0L) * 8 * 4.0 * LED_COUNT / 1000000000.0);
        //     printf("diff: %llu us, freq: %f, maxFreq: %f\n", diff, freq, maxFreq);
        //     time = now;
		// 	// These are the measures frequencies for different programs for 4 meters of 144 leds (4*144 total leds).
		// 	// max (theory):	47.176933 (1.0 / ((T0H + T0L) * 8 * 4 * 4 * 144.0 / 1000000000.0))
		// 	// no program:		48.790462
		// 	// rainbow:			44.771416
		// 	// white:			46.870377
		// 	// default:			46.370720 (this is lower than white, probably because the switch 'default' label is more expensive)
        // }

//        vTaskDelay(xDelay);
    }

	vm_free(vm);
    vTaskDelete(NULL);
}
