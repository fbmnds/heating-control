#include "gpio.h"

int gpio(int pin, int val)
{
    const char *chipname = "gpiochip0";
    const char *linename = "GPIO";
    const int lineoffset = pin;
    struct gpiod_chip *chip;
    struct gpiod_line *gpioPin;
    
 
    // Open GPIO chip
    chip = gpiod_chip_open_by_name(chipname);
    if (!chip) {
        perror("Open chip failed");
        return -1;
    }
 
    // Open GPIO line
    gpioPin = gpiod_chip_get_line(chip, lineoffset);
    if (!gpioPin) {
        fprintf(stderr, "Cannot find line with name: %s\n", linename);
        gpiod_chip_close(chip);
        return -1;
    }
 
    // Open GPIO line for output
    if (gpiod_line_request_output(gpioPin, "example1", val) < 0) {
        perror("Request line as output failed");
        gpiod_chip_close(chip);
        return -1;
    }
    
    // Release lines and chip
    gpiod_line_release(gpioPin);
    gpiod_chip_close(chip);
    return 0;
}


