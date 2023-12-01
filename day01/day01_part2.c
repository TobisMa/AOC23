#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char* getNextLine(FILE* f) {
    fpos_t filepos;
    int res = fgetpos(f, &filepos);
    if (res != 0) {
        printf("fgetpos failed\n");
        return NULL;
    }
    int lineLength = 0;
    while (!feof(f) && fgetc(f) != '\n') {
        lineLength++;
    }
    if (lineLength == 0) {
        return NULL;
    }
    lineLength += 1;  // terminating char '\0'
    char* line = malloc(sizeof(char) * lineLength);
    if (line == NULL) {
        printf("Failed to allocate memory");
        return NULL;
    }
    res = fsetpos(f, &filepos);
    if (res != 0) {
        printf("fsetpos failed\n");
        return NULL;
    }
    size_t chars = fread(line, sizeof(char), lineLength, f);  // appends an \0 char if res != NULL (no 0 char can be contained in the file)
    printf("Chars: %d\n", chars);

    line[lineLength - 1] = '\0';

    return line;
}

int getDigit(char* digit) {
    if (digit[0] <= '9' && digit[0] >= '0') {
        return digit[0] - '0';
    }
    else if (strncmp(digit, "one", 3) == 0) {
        return 1;
    }
    else if (strncmp(digit, "two", 3) == 0) {
        return 2;
    }
    else if (strncmp(digit, "three", 5) == 0) {
        return 3;
    }
    else if (strncmp(digit, "four", 4) == 0) {
        return 4;
    }
    else if (strncmp(digit, "five", 4) == 0) {
        return 5;
    }
    else if (strncmp(digit, "six", 3) == 0) {
        return 6;
    }
    else if (strncmp(digit, "seven", 5) == 0) {
        return 7;
    }
    else if (strncmp(digit, "eight", 5) == 0) {
        return 8;
    }
    else if (strncmp(digit, "nine", 4) == 0) {
        return 9;
    }
    return -1;
}

int main(int argc, char const *argv[])
{
    if (argc < 2) {
        printf("Please give filename\n");
        return EXIT_FAILURE;
    }
    const char* filename = argv[1];
    FILE* file = fopen(filename, "r");

    if (file == NULL) {
        printf("Failed to open file '%s'\n", filename);
        return EXIT_FAILURE;
    }

    char* line = NULL;
    int sum = 0;
    while ((line = getNextLine(file)) != NULL) {
        int i = 0;
        int first = -1;
        int last = -1;
        while (line[i] != '\0') {
            if (first == -1) {
                first = getDigit(line + i);
            }
            int tmp = getDigit(line + i);
            if (tmp != -1) {
                last = tmp;
            }
            i++;
        }
        free(line);
        line = NULL;  // remove dangling pointer
        if (i == 1) {
            break;
        }
        printf("Value: %i\n", first * 10 + last);
        if (first != -1 && last != -1) {
            sum += ((first * 10) + last);
        }
        else {
            printf("Ignored a line: %s; first=%i, last=%i\n", line, first, last);
        }
    }
    free(line);
    fclose(file);

    printf("Sum: %d\n", sum);
    return 0;
}
