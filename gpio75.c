#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main (int argc, char** argv) {
  char pin[] = "/sys/class/gpio/gpio75/value";
  FILE *fp;

  if (argc == 1) {
    system("/usr/bin/echo 75 > /sys/class/gpio/export");
    system("/usr/bin/echo 'out' > /sys/class/gpio/gpio75/direction");
    system("/usr/bin/echo 0 > /sys/class/gpio/gpio75/value");
  }

  if (argc == 2 && (argv[1][0] == '0' || argv[1][0] == '1')) {
    setuid(0);
    fp = fopen(pin, "w");
    if (fp) {
      fprintf(fp, "%c", argv[1][0]);
      fclose(fp);
    }
  }

  return 0;
}

// g++ -fpermissive -Wno-write-strings -o /usr/local/bin/gpio75 /usr/local/bin/gpio75.c
// chmod u+s /usr/local/bin/gpio75.c

// root crontab: @reboot /usr/local/bin/gpio75
