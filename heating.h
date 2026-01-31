#ifndef HEATING_H
#define HEATING_H

#include <gpiod.h>


#define DHT_OK               0
#define DHT_ERROR_ARG       -1
#define DHT_ERROR_CHECKSUM  -2
#define DHT_ERROR_TIMEOUT   -3
#define ERROR_GPIOCHIP      -4
#define ERROR_GPIOLINE      -5


typedef struct GPIO_pin { 
  char chipname[10];
  char linename[8];
  int lineoffset;
  struct gpiod_chip* chip;
  struct gpiod_line* line;
} GPIO_pin_t;


//extern GPIO_pin_t* dht_pin;
//extern GPIO_pin_t* heating_pin;

extern int pin_init(int pin_index, int gpio, int val);
extern void pin_close(int pin_index);

extern int dht(float*,float*);
extern int heat(int val);



#endif
