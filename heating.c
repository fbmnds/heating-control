/**

Written 2021 by Nigel Atkinson because the predominantly found C example for
reading a DHT11 just did not work.
Greatly inspired by the Adafruit python library for DHT11 & DHT22, however
simplified and using the library wiringPi.

Adafruit write good code! :-)

Tested on a Raspberry Pi 2B r1.1

Compile:
gcc dht11.c -o dht11 -lwiringPi

MIT License.
Copyright 2021 Nigel Atkinson

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in the
Software without restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
#include "heating.h"
#include "dbg.h"

#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <unistd.h>


struct GPIO_pin dht_pin = {
  "gpiochip0",
  "HEATCTL",
  -1,
  NULL,
  NULL
};

struct GPIO_pin heating_pin = {
  "gpiochip0",
  "HEATCTL",
  -1,
  NULL,
  NULL
};

GPIO_pin_t* pins_in_use[2] = { &dht_pin, &heating_pin};



int pin_init(int pin_index, int gpio, int val)
{
  GPIO_pin_t* pin = pins_in_use[pin_index];
  int err=0;

  debug("Try opening chip '%s' '%s' '%d'",
        pin->chipname, pin->linename, pin->lineoffset);

  // Re-use/open GPIO chip
  if (!pin->chip) {
    pin->chip = gpiod_chip_open_by_name(pin->chipname);
    if (!pin->chip) {
      debug("Open chip '%s' failed", pin->chipname);
      return ERROR_GPIOCHIP;
    }
  }
 
  // Re-/open GPIO line
  if (!pin->line) {
    pin->line = gpiod_chip_get_line(pin->chip, gpio);
    if (!pin->line) {
      debug("Cannot get line GPIO%d", gpio);
      gpiod_chip_close(pin->chip);
      return ERROR_GPIOLINE;
    }
    err = gpiod_line_request_output(pin->line, pin->linename, val);
    if (err<0) {
      debug("Cannot reserve line GPIO%d", gpio);
      gpiod_line_release(pin->line);
      gpiod_chip_close(pin->chip);
      return ERROR_GPIOLINE;
    }
  }

  pin->lineoffset = gpio;
  debug("OK opened chip '%x' '%x' '%d'", pin->chip, pin->line, pin->lineoffset);
  return 0;
}

void pin_close(int idx)
{
  // Release lines and chip
  gpiod_line_release(pins_in_use[idx]->line);
  gpiod_chip_close(pins_in_use[idx]->chip);
  debug("Released line %x and chip %x of %d",
        pins_in_use[idx]->line,
        pins_in_use[idx]->chip,
        pins_in_use[idx]->lineoffset);
  pins_in_use[idx]->lineoffset = -1;
}


/** 
 * How long to spin, waiting for input.
 */
#define DHT_MAXCOUNT 32000

/**
 * Number of bit pulses to expect from the DHT.  Note that this is 41 because
 * the first pulse is a constant 50 microsecond pulse, with 40 pulses to
 * represent the data afterwards.
 */
#define DHT_PULSES 41

int dht(float *humidity, float *temperature)
{
  int err=0, tries=5;
  float f=0.f;
  /* Array to store length of low and high pulses from the sensor */
  int pulse, pulseWidths[DHT_PULSES*2] = {0};
  uint32_t count = 0;

  uint8_t bytes[5];
  uint8_t bit;


  /* Make sure output pointers are probably ok */
  if (humidity == NULL || temperature == NULL ) {
    return DHT_ERROR_ARG;
  }
  
  if (!dht_pin.chip) return ERROR_GPIOCHIP;
  if (!dht_pin.line) return ERROR_GPIOLINE;

#define LINE dht_pin.line

 start:
  usleep(5000);

  err = 0;
  *humidity = 0.0f;
  *temperature = 0.0f;

  /* Signal sensor to output it's data. 
     Datasheet: LOW for ~500ms then HIGH for ~20ms */
  gpiod_line_set_direction_output(LINE,0);
  usleep(550);
  gpiod_line_set_value(LINE,1);
  usleep(20);
  /* Time the pulses coming in */
  gpiod_line_set_direction_input(LINE);
  /* Tiny delay to let pin stabilise as input pin and let voltage come up */
  for( volatile int i=0; i<25; i++);

  /* Wait for HIGH->LOW edge */
  count = 0;
  while (gpiod_line_get_value(LINE)) {
    if (++count > DHT_MAXCOUNT) {
      err=DHT_ERROR_TIMEOUT;
      goto error;
    }
    //for( volatile int i=0; i<5; i++);
  }

  /* Record pulse widths */
  pulse=0;
  while (pulse < DHT_PULSES*2) {
    /* Time low */
    while (!gpiod_line_get_value(LINE)) {
      if (++pulseWidths[pulse] > DHT_MAXCOUNT) {
        err=DHT_ERROR_TIMEOUT;
        goto error;
      }
    }
    ++pulse;
    /* Time high */
    while (gpiod_line_get_value(LINE)) {
      if (++pulseWidths[pulse] > DHT_MAXCOUNT) {
        err=DHT_ERROR_TIMEOUT;
        goto error;
      }
    }
    ++pulse;
  }

  /* Convert pulse widths to bits and bytes */
  for (int i=0;i<5;i++) { bytes[i] = 0; }
  bit = 0;
  pulse = 2; /* Skip over initial bit */
  while (pulse < DHT_PULSES*2) {
#ifdef DEBUG
    printf( 
           "Bit: %2d Byte: %2d Low: %3d High: %3d -> %1d  = 0x%2x\n", 
           bit,
           bit>>3,
           pulseWidths[pulse],
           pulseWidths[pulse+1],
           pulseWidths[pulse] <
           pulseWidths[pulse+1],
           bytes[bit>>3]
            );
    if (pulse % 16 == 0)
      puts("");
#endif

    bytes[bit>>3] <<= 1;
    if (pulseWidths[pulse] < pulseWidths[++pulse] ) {
      /* High part is longer than the preceding low, so this bit is a 1. */ 
      bytes[bit>>3] |= 1;
    }
    /* Otherwise high part is shorter, this bit is a 0 */

    ++bit;
    ++pulse;
  }

#ifdef DEBUG
  printf( "Data: %02x %02x %02x %02x Checksum: %02x : %02x\n",
          bytes[0],
          bytes[1],
          bytes[2],
          bytes[3],
          bytes[4],
          ((bytes[0] + bytes[1] + bytes[2] + bytes[3]) & 0xff)
          );
#endif
  
  f = ((int)bytes[0]) << 8 | bytes[1];
  f *= 0.1;
  *humidity = f;
  f = ((int)(bytes[2] & 0x7F)) << 8 | bytes[3];
  f *= 0.1;
  if (bytes[2] & 0x80 ){
    f *= -1;}
  *temperature = f;

  /* Check the checksum */
  if (bytes[4] != ((bytes[0] + bytes[1] + bytes[2] + bytes[3]) & 0xff)) {
    /* If debugging, keep outputs regardless of checksum validity */
#ifndef NDEBUG
    *humidity = -1;
    *temperature = -1;
#endif
    err=DHT_ERROR_CHECKSUM;
    goto error;
  }

 error:
    gpiod_line_set_direction_output(LINE,1);
    if (err<0) {
      if (tries>0) {
        tries--;
        goto start;
      } else {
        // dht_close();
        return err;
      }
    } else {
      // dht_close();
      return DHT_OK;
    }
}


int heat(int val)
{
  // Open GPIO line for output
  debug("heating_pin %x %x %x", heating_pin, heating_pin.chip, heating_pin.line);
  if (gpiod_line_set_value(heating_pin.line, val) < 0) {
    debug("Failed to set line GPIO%d to %d", heating_pin.lineoffset,val);
    return ERROR_GPIOLINE;
  }
  return 0;
}


