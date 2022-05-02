#include "sdkconfig.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_log.h"
#include "driver/rmt.h"
#include "esp_timer.h"
#include "ledz.h"
#include "esp_http_server.h"
#include "nvs_flash.h"
#include "esp_netif.h"
#include "protocol_examples_common.h"
#include "mdns.h"

#define CONFIG_MDNS_HOSTNAME	"Lamp"
#define EXAMPLE_MDNS_INSTANCE	"Lamp1"
static const char *TAG = "mdns-test";

static void initialise_mdns(void);
static httpd_handle_t start_webserver(void);

TaskHandle_t ledzTaskHandle;
SemaphoreHandle_t ledzTaskMutex;
int programCode[MAX_CODE_SIZE];

void app_main(void)
{
    ESP_ERROR_CHECK(nvs_flash_init());
    ESP_ERROR_CHECK(esp_netif_init());
    ESP_ERROR_CHECK(esp_event_loop_create_default());

    /* This helper function configures Wi-Fi or Ethernet, as selected in menuconfig.
     * Read "Establishing Wi-Fi or Ethernet Connection" section in
     * examples/protocols/README.md for more information about this function.
     */
    ESP_ERROR_CHECK(example_connect());

	ledzTaskMutex = xSemaphoreCreateMutex();
    initialise_mdns();
	start_webserver();

    xTaskCreatePinnedToCore(ledz_task, "ledz_task", 16384, NULL, 10, &ledzTaskHandle, 1);
}

/**
 * Generate host name based on sdkconfig, optionally adding a portion of MAC address to it.
 * @return host name string allocated from the heap
 */
static char* generate_hostname(void)
{
    uint8_t mac[6];
    char   *hostname;
    esp_read_mac(mac, ESP_MAC_WIFI_STA);
    if (-1 == asprintf(&hostname, "%s-%02X%02X%02X", CONFIG_MDNS_HOSTNAME, mac[3], mac[4], mac[5])) {
        abort();
    }
    return hostname;
}

static void initialise_mdns(void)
{
    char* hostname = generate_hostname();
    //initialize mDNS
    ESP_ERROR_CHECK(mdns_init());
    //set mDNS hostname (required if you want to advertise services)
    ESP_ERROR_CHECK(mdns_hostname_set(hostname));
    ESP_LOGI(TAG, "mdns hostname set to: [%s]", hostname);
    //set default mDNS instance name
    ESP_ERROR_CHECK(mdns_instance_name_set(EXAMPLE_MDNS_INSTANCE));
    //initialize service
    ESP_ERROR_CHECK(mdns_service_add("LampServer", "_lamp", "_tcp", 80, NULL, 0));
    //add another TXT item
    ESP_ERROR_CHECK(mdns_service_txt_item_set("_lamp", "_tcp", "path", "/"));
    free(hostname);
}

/**
 * Our URI handler function to be called during POST /uri request
 */
static esp_err_t post_handler(httpd_req_t *req)
{
    // /* Destination buffer for content of HTTP POST request.
    //  * httpd_req_recv() accepts char* only, but content could
    //  * as well be any binary data (needs type casting).
    //  * In case of string data, null termination will be absent, and
    //  * content length would give length of string */
    // char content[100];

    // /* Truncate if content length larger than the buffer */
    // size_t recv_size = MIN(req->content_len, sizeof(content));

//	char *content = malloc(req->content_len);

	ESP_LOGI(TAG, "Content Length %d", req->content_len);

	ESP_LOGI(TAG, "Trying to pause task");
	// Try to pause the ledz task and get a lock on the data
	xTaskNotify(ledzTaskHandle, PAUSE_TASK, eSetBits);

	if (xSemaphoreTake(ledzTaskMutex, pdMS_TO_TICKS(250)) != pdTRUE) {
		// We could not obtain the semaphore and can therefore not access the shared resource safely.
		ESP_LOGE(TAG, "Could not get a lock on the vm.");
        httpd_resp_send_err(req, HTTPD_500_INTERNAL_SERVER_ERROR, "{\"message\": \"Could not get a lock on the vm.\"}");
		return ESP_FAIL;
	}

	if (req->content_len > MAX_CODE_SIZE * 4) {
		httpd_resp_send_err(req, HTTPD_400_BAD_REQUEST, "{\"message\": \"The program is too large.\"}");
		return ESP_FAIL;
	}

	// for(int j = 0; j < 10; j++) {
    //     printf("%d ", programCode[j]);
    // }
	// printf("\n");

	int ret = httpd_req_recv(req, programCode, req->content_len);
	if (ret <= 0) {  /* 0 return value indicates connection closed */
		/* Check if timeout occurred */
		if (ret == HTTPD_SOCK_ERR_TIMEOUT) {
			/* In case of timeout one can choose to retry calling
			* httpd_req_recv(), but to keep it simple, here we
			* respond with an HTTP 408 (Request Timeout) error */
			httpd_resp_send_408(req);
		}
		/* In case of error, returning ESP_FAIL will
		* ensure that the underlying socket is closed */
		return ESP_FAIL;
	}

	// for(int j = 0; j < 10; j++) {
    //     printf("%d ", programCode[j]);
    // }
	// printf("\n");

	ESP_LOGI(TAG, "Trying to resume task");

	xSemaphoreGive(ledzTaskMutex);
	xTaskNotify(ledzTaskHandle, RESUME_TASK, eSetBits);

    /* Send a simple response */
    const char resp[] = "Did it!";
    httpd_resp_send(req, resp, HTTPD_RESP_USE_STRLEN);
    return ESP_OK;
}

/* URI handler structure for POST /uri */
static httpd_uri_t uri_post = {
    .uri      = "/run-program",
    .method   = HTTP_POST,
    .handler  = post_handler,
    .user_ctx = NULL
};

/* Function for starting the webserver */
static httpd_handle_t start_webserver(void)
{
    /* Generate default configuration */
    httpd_config_t config = HTTPD_DEFAULT_CONFIG();

    /* Empty handle to esp_http_server */
    httpd_handle_t server = NULL;

    /* Start the httpd server */
    if (httpd_start(&server, &config) == ESP_OK) {
        /* Register URI handlers */
        httpd_register_uri_handler(server, &uri_post);
    }
    /* If server failed to start, handle will be NULL */
    return server;
}
