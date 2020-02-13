#ifndef __CAP_CSV_FILE_H__
#define __CAP_CSV_FILE_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct CSVRow {
    char** entries;
    int entryCount;

    struct CSVRow* prev;
    struct CSVRow* next;
} CSVRow;

typedef struct {
    const char* filepath;
    CSVRow* currentRow;
} CSVFile;

extern CSVRow CSV_NULL_ROW;
extern CSVRow* CSV_NULL_ROW_PTR;

CSVFile csvLoadFile(const char* filepath);

CSVRow* csvNextRow(CSVFile* csvFile);
CSVRow* csvPrevRow(CSVFile* csvFile);

CSVRow* csvSeekEnd(CSVFile* csvFile);
CSVRow* csvSeekBegin(CSVFile* csvFile);

char** csvTokenizeLine(char* linebase, int* tokenCount);

// calling csvFreeRow should be avoided; let the CSVFile handle
// management of the rows
void csvFreeRow(CSVRow* row);
void csvFreeFile(CSVFile* csvFile);


#endif /* __CAP_CSV_FILE_H__ */
